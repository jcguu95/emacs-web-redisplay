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
    const output = await window.electronAPI.click()
    console.log("output: ", output)
    const result_ = JSON.parse(output)
    console.log("result_: ", result_)
    const result = JSON.parse(result_)
    console.log("result: ", result) // A json list, each of whose entries is a json dict.
    emacsWindow = document.querySelectorAll('.emacs-window')[0];
    emacsWindow.textContent = '';
    for (let i = 0; i < result.length; i++) {
        span = document.createElement('span');
        console.log(result[i]['text'])
        span.textContent = result[i]['text'];
        emacsWindow.appendChild(span);
    }
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
