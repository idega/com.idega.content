package com.idega.content.tree;

import com.idega.business.IBOService;

public interface PagePreview extends IBOService{
	
	public String getPreviewUrl(String ID);

}