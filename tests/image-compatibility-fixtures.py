"""Create small PNG/BMP inputs and independent RGBA8 references.

Each .rgba begins with little-endian uint32 width and height. The expected
pixels come from the logical samples, before encoding rows or pass geometry.
"""

from pathlib import Path
import struct
import sys
import zlib

directory = Path(sys.argv[1])
directory.mkdir(parents=True, exist_ok=True)
WIDTH, HEIGHT = 23, 13
ADAM7 = ((0, 0, 8, 8), (4, 0, 8, 8), (0, 4, 4, 8), (2, 0, 4, 4),
         (0, 2, 2, 4), (1, 0, 2, 2), (0, 1, 1, 2))


def reference(stem, width, height, pixels):
    (directory / f"{stem}.rgba").write_bytes(
        struct.pack("<II", width, height) + bytes(component for row in pixels for component in row)
    )


def pack_row(samples, depth):
    if depth >= 8:
        return b"".join(sample.to_bytes(depth // 8, "big") for sample in samples)
    result = bytearray((len(samples) * depth + 7) // 8)
    for i, sample in enumerate(samples):
        offset, bit = divmod(i * depth, 8)
        result[offset] |= sample << (8 - bit - depth)
    return bytes(result)


def paeth(left, up, corner):
    predicted = left + up - corner
    return min((left, up, corner), key=lambda x: abs(x - predicted))


def filtered(row, prior, bpp, method):
    encoded = bytearray()
    for i, value in enumerate(row):
        left = row[i - bpp] if i >= bpp else 0
        up = prior[i]
        corner = prior[i - bpp] if i >= bpp else 0
        predictors = (0, left, up, (left + up) // 2, paeth(left, up, corner))
        encoded.append((value - predictors[method]) & 255)
    return bytes([method]) + encoded


def chunk(kind, data):
    return struct.pack(">I", len(data)) + kind + data + struct.pack(">I", zlib.crc32(kind + data))


def png(stem, depth, color, interlace, width=WIDTH, height=HEIGHT):
    channels = {0: 1, 2: 3, 3: 1, 4: 2, 6: 4}[color]
    maximum = (1 << depth) - 1
    samples = [[tuple((x * 211 + y * 503 + channel * 12345) & maximum for channel in range(channels))
                for x in range(width)] for y in range(height)]
    transparent = samples[0][min(3, width - 1)]
    palette = [(i * 71 & 255, i * 103 & 255, i * 151 & 255) for i in range(1 << depth)] if color == 3 else []
    palette_alpha = [i * 251 & 255 for i in range(max(1, len(palette) - 1))]
    header = struct.pack(">IIBBBBB", width, height, depth, color, 0, 0, interlace)
    image = b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", header)
    if color in (0, 2):
        image += chunk(b"tRNS", b"".join(sample.to_bytes(2, "big") for sample in transparent))
    if color == 3:
        image += chunk(b"PLTE", bytes(channel for entry in palette for channel in entry))
        image += chunk(b"tRNS", bytes(palette_alpha))
    data = bytearray()
    for x0, y0, dx, dy in ADAM7 if interlace else ((0, 0, 1, 1),):
        if x0 >= width or y0 >= height:
            continue
        prior = None
        for y in range(y0, height, dy):
            row = pack_row([value for pixel in samples[y][x0::dx] for value in pixel], depth)
            data.extend(filtered(row, prior if prior is not None else bytes(len(row)),
                                 max(1, (channels * depth + 7) // 8), y % 5))
            prior = row
    payload = zlib.compress(data, level=9)
    image += chunk(b"IDAT", payload[:len(payload) // 2]) + chunk(b"IDAT", payload[len(payload) // 2:]) + chunk(b"IEND", b"")
    (directory / f"{stem}.png").write_bytes(image)

    def convert(pixel):
        if color == 3:
            index, = pixel
            return (*palette[index], palette_alpha[index] if index < len(palette_alpha) else 255)
        values = tuple(value >> 8 if depth == 16 else value * 255 // maximum for value in pixel)
        alpha = 0 if pixel == transparent else 255
        if color == 0:
            return (values[0],) * 3 + (alpha,)
        if color == 2:
            return (*values, alpha)
        if color == 4:
            return (values[0],) * 3 + (values[1],)
        return values

    reference(stem, width, height, (convert(pixel) for row in samples for pixel in row))


def bmp(stem, depth, mode="rgb", top_down=False, core=False):
    palette = [(i * 37 & 255, i * 119 & 255, i * 151 & 255) for i in range(1 << depth)] if depth < 16 else []
    entry_bytes = 3 if core else 4
    table = b"".join(bytes([b, g, r, 0][:entry_bytes]) for r, g, b in palette)
    mask = {"rgb": (), "565": (0xf800, 0x07e0, 0x001f), "rgba": (0xff, 0xff00, 0xff0000, 0xff000000)}[mode]
    compression = 3 if mask else 0
    if core:
        header = struct.pack("<IHHHH", 12, WIDTH, HEIGHT, 1, depth)
    else:
        header = struct.pack("<IiiHHIIiiII", 108 if mode == "rgba" else 40, WIDTH,
                             -HEIGHT if top_down else HEIGHT, 1, depth, compression, 0, 0, 0, len(palette), 0)
        header += b"".join(struct.pack("<I", m) for m in mask)
        if mode == "rgba":
            header += bytes(108 - len(header))

    rows, expected = [], []
    for y in range(HEIGHT):
        data, decoded = [], []
        for x in range(WIDTH):
            seed = x * 37 + y * 19
            if depth < 16:
                index = seed & ((1 << depth) - 1)
                data.append(index)
                decoded.append((*palette[index], 255))
            elif depth == 16:
                r, g, b = seed & 31, (seed * 3) & (63 if mode == "565" else 31), (seed * 5) & 31
                data.append((r << (11 if mode == "565" else 10)) | g << 5 | b)
                g8 = (g << 2 | g >> 4) if mode == "565" else (g << 3 | g >> 2)
                decoded.append((r << 3 | r >> 2, g8, b << 3 | b >> 2, 255))
            else:
                r, g, b = seed & 255, (seed * 3) & 255, (seed * 5) & 255
                alpha = 0 if stem.endswith("-unused-alpha") else (seed * 7) & 255
                if depth == 24:
                    data.extend((b, g, r))
                    decoded.append((r, g, b, 255))
                else:
                    data.extend((r, g, b, alpha) if mode == "rgba" else (b, g, r, alpha))
                    decoded.append((r, g, b, alpha if not stem.endswith("-unused-alpha") else 255))
        if depth == 16:
            row = b"".join(value.to_bytes(2, "little") for value in data)
        elif depth < 16:
            row = pack_row(data, depth)
        else:
            row = bytes(data)
        rows.append(row + bytes(-len(row) % 4))
        expected.extend(decoded)
    body = b"".join(rows if top_down else rows[::-1])
    offset = 14 + len(header) + len(table)
    file_header = struct.pack("<2sIHHI", b"BM", offset + len(body), 0, 0, offset)
    (directory / f"{stem}.bmp").write_bytes(file_header + header + table + body)
    reference(stem, WIDTH, HEIGHT, expected)


for color, depths in ((0, (1, 2, 4, 8, 16)), (2, (8, 16)), (3, (1, 2, 4, 8)), (4, (8, 16)), (6, (8, 16))):
    for depth in depths:
        for interlace in (0, 1):
            png(f"png-{color}-{depth}-{interlace}", depth, color, interlace)
png("png-tiny", 2, 3, 1, width=1, height=1)
for depth in (1, 4, 8):
    bmp(f"bmp-{depth}", depth)
    bmp(f"bmp-{depth}-core", depth, core=True)
bmp("bmp-16-rgb555", 16)
bmp("bmp-16-rgb565", 16, mode="565")
bmp("bmp-24", 24)
bmp("bmp-24-topdown", 24, top_down=True)
bmp("bmp-32", 32)
bmp("bmp-32-unused-alpha", 32)
bmp("bmp-32-alpha-mask", 32, mode="rgba")
