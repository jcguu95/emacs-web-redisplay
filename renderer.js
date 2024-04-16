const btn = document.getElementById('btn')
// const windows = document.querySelectorAll('.window');

// windows[0].classList.add('active');
// windows.forEach((div, index) => {
//     div.addEventListener('click', () => {
//         windows.forEach(d => d.classList.remove('active'));
//         div.classList.add('active');
//     });
// });

// IPC with emacs via WebSocket
// FIXME When connection fails, warn about the situation, and try to reconnect.
emacs_ws = new WebSocket("ws://localhost:3000");
emacs_ws.onmessage = function(event) {
  console.log(event);
  document.getElementsByClassName("emacs-window")[0].textContent = JSON.parse(event.data)[0]
}




// Checks Connection with Emacs //
async function checkEmacsConnectionLoop() {
  while (true) {
    await new Promise(resolve => setTimeout(resolve, 1000)); // sleep for 1 second
      if (emacs_ws.readyState === WebSocket.OPEN) {
          document.getElementById('emacs-connection-status').textContent = 'Connected';
      } else {
          console.log("Disconnected!")
          document.getElementById('emacs-connection-status').textContent = 'Disconnected';
      }
  }
}
checkEmacsConnectionLoop();




btn.addEventListener('click', async () => {
    console.log("OUCH!")
    emacs_ws.send("JavaScript: Hello!");
    // emacs_ws.close();
})




// // Add event listener for keyup event
// document.addEventListener('keyup', function(event) {
//     const activeWindow = document.querySelector('.window.active');
//     let currentText = activeWindow.textContent;
//     if (event.key === 'Backspace') {
//         // Remove the last character if backspace is pressed
//         activeWindow.textContent = currentText.slice(0, -1);
//     } else {
//         activeWindow.textContent = currentText + event.key;
//     }
// });


// TODO Fix the problem with
//
// document.addEventListener('keydown', (event) => {
//     console.log(event)
//     console.log(event.keyCode)
// });

// Function Keys
const functionKeysMap = {"Control": false,
                         "Alt": false,
                         "Shift": false,
                         "Meta": false,
                         "Hyper": false}

function modalEdit(key) {
    if (functionKeysMap["Hyper"]) { key = "H-" + key}
    if (functionKeysMap["Meta"]) { key = "s-" + key}
    if (functionKeysMap["Shift"]) { key = "S-" + key}
    if (functionKeysMap["Alt"]) { key = "M-" + key}
    if (functionKeysMap["Control"]) { key = "C-" + key}
    return key
}

document.addEventListener("keydown", function(event) {
    switch (event.key) {
    case "Control":
        functionKeysMap["Control"] = true;
        break;
    case "Shift":
        functionKeysMap["Shift"] = true;
        break;
    case "Alt":
        functionKeysMap["Alt"] = true;
        break;
    case "Meta":
        functionKeysMap["Meta"] = true;
        break;
    case "Hyper":
        functionKeysMap["Hyper"] = true;
        break;
    default:
        console.log(functionKeysMap);
        emacs_ws.send(`[${new Date(Date.now()).toISOString()}] JS: ${modalEdit(event.key)}`);
    }
});

document.addEventListener("keyup", function(event) {
    switch (event.key) {
    case "Control":
        functionKeysMap["Control"] = false;
    case "Shift":
        functionKeysMap["Shift"] = false;
    case "Alt":
        functionKeysMap["Alt"] = false;
    case "Meta":
        functionKeysMap["Meta"] = false;
    case "Hyper":
        functionKeysMap["Hyper"] = false;
    }
});
