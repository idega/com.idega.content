function ThemesPreviewsProvider() { }

ThemesPreviewsProvider._path = '/dwr';

ThemesPreviewsProvider.getImagesInfo = function(callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'getImagesInfo', callback);
}