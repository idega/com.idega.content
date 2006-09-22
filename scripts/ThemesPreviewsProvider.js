function ThemesPreviewsProvider() { }

ThemesPreviewsProvider._path = '/dwr';

ThemesPreviewsProvider.getImagesInfo = function(callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'getImagesInfo', callback);
}

ThemesPreviewsProvider.compareNodes = function(p0,callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'compareNodes', p0, callback);
}