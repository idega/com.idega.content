function ThemesPreviewsProvider() { }

ThemesPreviewsProvider._path = '/dwr';

ThemesPreviewsProvider.getThemesPreviewsInfo = function(callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'getThemesPreviewsInfo', callback);
}

ThemesPreviewsProvider.getPagePreview = function(callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'getPagePreview', callback);
}

ThemesPreviewsProvider.getThemeStyleVariations = function(p0, callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'getThemeStyleVariations', p0, callback);
}

ThemesPreviewsProvider.changeTheme = function(p0, p1, p2, p3, p4, callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'changeTheme', p0, p1, p2, p3, p4, callback);
}

ThemesPreviewsProvider.saveTheme = function(p0, p1, callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'saveTheme', p0, p1, callback);
}