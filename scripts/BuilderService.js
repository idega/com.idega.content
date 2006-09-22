function BuilderService() { }

BuilderService._path = '/dwr';

ThemesPreviewsProvider.getImagesInfo = function(callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'ThemesPreviewsProvider', 'getImagesInfo', callback);
}

BuilderService.moveTreeNodes = function(p0,callback) {
    DWREngine._execute(ThemesPreviewsProvider._path, 'BuilderService', 'moveTreeNodes', p0, callback);
}