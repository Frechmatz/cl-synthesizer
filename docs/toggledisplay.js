
let toggleDisplay = function(textShow, textHide, linkId, contentId) {
    let contentElement = document.getElementById(contentId);
    if (contentElement.style.display == 'block') {
	document.getElementById(linkId).textContent=textShow;
	contentElement.style.display = 'none';
    }
    else {
	document.getElementById(linkId).textContent=textHide;
	contentElement.style.display = 'block';
    }
    return false; // prevent default
};
