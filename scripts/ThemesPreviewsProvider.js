function ThemesPreviewsProvider() { }

ThemesPreviewsProvider._path = '/dwr';

ThemesPreviewsProvider.getThemesPreviewsInfo = function(callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'getThemesPreviewsInfo', callback);
}

ThemesPreviewsProvider.getPagePreview = function(callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'getPagePreview', callback);
}