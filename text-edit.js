const windows = document.querySelectorAll('.window');
console.log(windows)

windows[0].classList.add('active');
windows.forEach((div, index) => {
    console.log('Handling div: ', div);
    div.addEventListener('click', () => {
        windows.forEach(d => d.classList.remove('active'));
        div.classList.add('active');
    });
});

// Add event listener for keyup event
document.addEventListener('keyup', function(event) {
    console.log("keyup", event)
    const activeWindow = document.querySelector('.window.active');
    console.log('active window:', activeWindow)
    let currentText = activeWindow.textContent;
    console.log('current text:', currentText)
    if (event.key === 'Backspace') {
        // Remove the last character if backspace is pressed
        activeWindow.textContent = currentText.slice(0, -1);
    } else {
        // Append the pressed character to the div's text
        console.log('Appending ' + event.key);
        activeWindow.textContent = currentText + event.key;
    }
});
