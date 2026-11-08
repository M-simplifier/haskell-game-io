const inputDemo = document.querySelector("[data-input-demo]");
const inputSteps = [...inputDemo.querySelectorAll("[data-input-at]")];
const inputTime = inputDemo.querySelector("[data-input-time]");
const inputState = inputDemo.querySelector("[data-input-state]");
const inputEvents = inputDemo.querySelector("[data-input-events]");
const inputNext = inputDemo.querySelector("[data-input-next]");
const inputReset = inputDemo.querySelector("[data-input-reset]");
let inputStep = 0;

function showInputStep(index) {
  inputStep = index;
  for (const [i, step] of inputSteps.entries()) {
    if (i === index) step.setAttribute("aria-current", "step");
    else step.removeAttribute("aria-current");
  }
  const milliseconds = Number(inputSteps[index].dataset.inputAt);
  inputTime.textContent = `${milliseconds} ms`;
  inputState.textContent = milliseconds === 3 ? "キーを押している" : "キーは離れている";
  inputEvents.textContent = [
    "受け付けた変化はまだありません。",
    "受け付けた変化は押下1件です。",
    "押下と解放を受け付けました。",
    "押下と解放の2件を、この順でゲームに渡します。",
  ][index];
  inputNext.setAttribute("aria-disabled", String(index === inputSteps.length - 1));
  inputReset.setAttribute("aria-disabled", String(index === 0));
}

inputNext.addEventListener("click", () => {
  if (inputStep + 1 < inputSteps.length) showInputStep(inputStep + 1);
});
inputReset.addEventListener("click", () => {
  if (inputStep !== 0) showInputStep(0);
});
showInputStep(0);
inputDemo.querySelector("[data-input-controls]").hidden = false;

const audioDemo = document.querySelector("[data-audio-demo]");
const audioSplit = audioDemo.querySelector("#audio-split");
const audioTrack = audioDemo.querySelector("[data-audio-track]");
const audioBlocks = audioDemo.querySelector("[data-audio-blocks]");
const totalFrames = 480;

function showAudioSplit() {
  const before = audioSplit.valueAsNumber;
  const after = totalFrames - before;
  audioTrack.style.setProperty("--block-split", `${100 * before / totalFrames}%`);
  audioBlocks.textContent = `${before} ＋ ${after}フレーム`;
  audioSplit.setAttribute("aria-valuetext", `前半${before}フレーム、後半${after}フレーム`);
}

audioSplit.addEventListener("input", showAudioSplit);
showAudioSplit();
audioDemo.querySelector("[data-audio-controls]").hidden = false;
