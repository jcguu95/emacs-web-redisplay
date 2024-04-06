const btn = document.getElementById('btn')
const windows = document.querySelectorAll('.window');

windows[0].classList.add('active');
windows.forEach((div, index) => {
    div.addEventListener('click', () => {
        windows.forEach(d => d.classList.remove('active'));
        div.classList.add('active');
    });
});

btn.addEventListener('click', async () => {
    const result = await window.electronAPI.click()
    console.log('main->', result)
})

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
