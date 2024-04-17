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
  document.getElementsByClassName("emacs-window")[0].textContent = "";
  data = JSON.parse(event.data);
  text = data["text"];
  pt = data["effective-point"]; counter = 1; // for drawing cursor
  text.forEach ( function(character) {
    var newSpan = document.createElement("span");
    newSpan.textContent = character["c"]
    newSpan.style.color = character["fg"]
    // draws cursor
    if (counter == pt) {
      newSpan.style.color = "black";
      newSpan.style.backgroundColor = "white";
    }
    document.getElementsByClassName("emacs-window")[0].appendChild(newSpan);
    counter += 1;
  });

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

// btn.addEventListener('click', async () => {
//     console.log("OUCH!")
//     emacs_ws.send("JavaScript: Hello!");
//     // emacs_ws.close();
// })


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

function event2keyname (event) {
  console.log(event)
  var realKey = ""

  if (event.shiftKey) {
    realKey = event.key
  } else {
    realKey = event.code
  }

  if (realKey.match("^Key")) {
    realKey = realKey.substring(3).toLowerCase()
  } else if (realKey.match("^F")) {
    // <f1> ~ <f12>
    realKey = "<f" + realKey.substring(1) + ">"
  } else if (realKey.match("^Digit")) {
    // Digits
    realKey = realKey.substring(5)
  } else if (realKey.match("^Arrow")) {
    // Arrows
    realKey = "<" + realKey.substring(5).toLowerCase() + ">"
  } else if (realKey.match("Escape")) {
    realKey = "ESC"
  } else if (realKey.match("Enter")) {
    realKey = "RET"
  } else if (realKey.match("Slash")) {
    realKey = "/"
  } else if (realKey.match("Space")) {
    realKey = "SPC"
  } else if (realKey.match("Backspace")) {
    realKey = "DEL"
  } else {
    realKey = realKey
  }

  // Decoration with function keys
  if (event.ctrlKey) {realKey = "C-" + realKey} ;
  if (event.metaKey) {realKey = "s-" + realKey} ;
  if (event.altKey)  {realKey = "M-" + realKey} ;

  // Debug
  // console.log(realKey) ;
  return realKey ;
}

document.addEventListener("keydown", function(event) {
    if (["Alt", "Shift", "Control", "Meta"].includes(event.key)) {
        return;
    }
    emacs_ws.send(`${event2keyname(event)}`);
});


