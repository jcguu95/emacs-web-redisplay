const btn = document.getElementById('btn')
const windows = document.querySelectorAll('.window');

windows[0].classList.add('active');
windows.forEach((div, index) => {
    div.addEventListener('click', () => {
        windows.forEach(d => d.classList.remove('active'));
        div.classList.add('active');
    });
});

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




document.addEventListener('keydown', function(event) {
    emacs_ws.send(`[${new Date(Date.now()).toISOString()}] JS: ${event.key}`);
});



// Add event listener for keyup event
document.addEventListener('keyup', function(event) {
    const activeWindow = document.querySelector('.window.active');
    let currentText = activeWindow.textContent;
    if (event.key === 'Backspace') {
        // Remove the last character if backspace is pressed
        activeWindow.textContent = currentText.slice(0, -1);
    } else {
        activeWindow.textContent = currentText + event.key;
    }
});
