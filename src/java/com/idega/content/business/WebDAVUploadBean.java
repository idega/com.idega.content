package com.idega.content.business;

import java.io.IOException;
import javax.faces.context.FacesContext;
import net.sourceforge.myfaces.custom.fileupload.UploadedFile;

public class WebDAVUploadBean{
	
	private UploadedFile _upFile;
	private String _name = "";

	public UploadedFile getUpFile() {
		return _upFile;
	}

	public void setUpFile(UploadedFile upFile){
		System.out.println("Setting file = " + upFile.getName());
		_upFile = (UploadedFile)upFile;
	}

	public String getName(){
		return _name;
	}

	public void setName(String name) {
		_name = name;
	}

	public String upload() throws IOException{

		FacesContext facesContext = FacesContext.getCurrentInstance();

		facesContext.getExternalContext().getApplicationMap().put(
				"fileupload_bytes", _upFile.getBytes());

		facesContext.getExternalContext().getApplicationMap().put(
				"fileupload_type", _upFile.getContentType());

		facesContext.getExternalContext().getApplicationMap().put(
				"fileupload_name", _upFile.getName());
		return "ok";

	}

	public boolean isUploaded(){
		FacesContext facesContext = FacesContext.getCurrentInstance();
		return facesContext.getExternalContext().getApplicationMap().get(
				"fileupload_bytes") != null;
	}
}