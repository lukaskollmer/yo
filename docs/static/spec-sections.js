function LKCreateElement(tagName, attributes) {
    const elem = document.createElement(tagName);
    for (const [key, value] of Object.entries(attributes)) {
        switch (key) {
        case 'classList':
            elem.classList.add(...value);
            break;
        case 'children': 
            Array.from(value).forEach(node => elem.appendChild(node));
            break;
        default:
            elem[key] = value;
            break;
        }
    }
    return elem;
}

(() => {
    const toc = document.getElementById('toc');

    for (const heading of document.querySelectorAll('h1, h2, h3, h4, h5, h6')) {
        const sectionId = heading.getAttribute('sectionId');
        if (sectionId === null) continue;

        const sectionName = heading.textContent;
        const sectionHref = `#${sectionId}`;
        heading.id = sectionId;
        heading.innerHTML = '';
    
        heading.appendChild(LKCreateElement('a', {
            classList: ['casual'],
            href: sectionHref,
            textContent: sectionName
        }))


        heading.appendChild(LKCreateElement('span', {
            classList: ['section-id'],
            children: [
                LKCreateElement('a', {
                    href: sectionHref,
                    textContent: `[${sectionId}]`
                })
            ]
        }));

        toc.appendChild(LKCreateElement('div', {
            children: [
                LKCreateElement('a', {
                    classList: ['casual', 'toc-entry', heading.tagName.toLowerCase()],
                    href: sectionHref,
                    textContent: sectionName
                })
            ]
        }))
    }
})();
