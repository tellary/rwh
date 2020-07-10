// https://zocada.com/compress-resize-images-javascript-browser/
function ean13_resize(data, resolve) {
    const width = 380;
    const img = new Image();
    img.src = data;
    const height = (width/img.width)*img.height;
    img.onload = () => {
        const elem = document.createElement('canvas');
        elem.width = width;
        elem.height = height;
        const ctx = elem.getContext('2d');
        // img.width and img.height will contain the original dimensions
        ctx.drawImage(img, 0, 0, width, height);
        ctx.canvas.toBlob((blob) => {
            console.log("blob:" + blob);
            // It may be `null` weirdly enough. Let's keep trying.
            if (blob == null) {
                ean13_resize(data, resolve);
            } else {
                const file = new File([blob], "tmpFile", {
                    type: 'image/jpeg',
                    lastModified: Date.now()
                });
                r = new FileReader();
                r.readAsDataURL(file);
                r.onload = e => resolve(e.target.result);
            }
        }, 'image/jpeg', 1);
    };
}
