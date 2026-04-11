function toggleTheme() {
    var root = document.documentElement;
    var currentTheme = root.getAttribute('data-bs-theme');
    var newTheme = currentTheme === 'dark' ? 'light' : 'dark';
    root.setAttribute('data-bs-theme', newTheme);
    localStorage.setItem('theme', newTheme);
    var btn = document.getElementById('theme-toggle');
    btn.innerHTML = '\u25d1';
}

document.addEventListener("DOMContentLoaded", function() {
    var storedTheme = localStorage.getItem('theme') || 'light';
    document.documentElement.setAttribute('data-bs-theme', storedTheme);
    var btn = document.getElementById('theme-toggle');
    btn.innerHTML = '\u25d1';
});
