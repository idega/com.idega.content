function PagePreview() { }

PagePreview._path = '/dwr';

PagePreview.getPreviewUrl = function(p0, callback) {
    DWREngine._execute(PagePreview._path, 'PagePreview', 'getPreviewUrl', p0, callback);
}