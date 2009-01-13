package com.idega.content.lucid.bean;

import java.util.List;

public class LucidApplicationInfo {

	private List<String> localizedTexts;
	
	private String pageId;
	private String classNameForSourceView;
	private String pathToImageFolder;
	private String pageUri;
	private String mooRainbowImage;
	
	private boolean contentEditor;
	private boolean startPage;
	private boolean canActAsBuilderUser;
	
	private String[] pageInfoElements;

	public List<String> getLocalizedTexts() {
		return localizedTexts;
	}

	public void setLocalizedTexts(List<String> localizedTexts) {
		this.localizedTexts = localizedTexts;
	}

	public String getPageId() {
		return pageId;
	}

	public void setPageId(String pageId) {
		this.pageId = pageId;
	}

	public String getClassNameForSourceView() {
		return classNameForSourceView;
	}

	public void setClassNameForSourceView(String classNameForSourceView) {
		this.classNameForSourceView = classNameForSourceView;
	}

	public String getPathToImageFolder() {
		return pathToImageFolder;
	}

	public void setPathToImageFolder(String pathToImageFolder) {
		this.pathToImageFolder = pathToImageFolder;
	}

	public String getPageUri() {
		return pageUri;
	}

	public void setPageUri(String pageUri) {
		this.pageUri = pageUri;
	}

	public boolean isContentEditor() {
		return contentEditor;
	}

	public void setContentEditor(boolean contentEditor) {
		this.contentEditor = contentEditor;
	}

	public boolean isStartPage() {
		return startPage;
	}

	public void setStartPage(boolean startPage) {
		this.startPage = startPage;
	}

	public boolean isCanActAsBuilderUser() {
		return canActAsBuilderUser;
	}

	public void setCanActAsBuilderUser(boolean canActAsBuilderUser) {
		this.canActAsBuilderUser = canActAsBuilderUser;
	}

	public String[] getPageInfoElements() {
		return pageInfoElements;
	}

	public void setPageInfoElements(String[] pageInfoElements) {
		this.pageInfoElements = pageInfoElements;
	}

	public String getMooRainbowImage() {
		return mooRainbowImage;
	}

	public void setMooRainbowImage(String mooRainbowImage) {
		this.mooRainbowImage = mooRainbowImage;
	}
	
}
