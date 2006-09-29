function NodeID() { }

NodeID._path = '/dwr';

NodeID.OldIDsToJavaScript = function(p0,callback) {
    DWREngine._execute(NodeID._path, 'NodeID', 'OldIDsToJavaScript', callback);
}