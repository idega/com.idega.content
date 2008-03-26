package com.idega.content.themes.helpers.business;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.rmi.RemoteException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.httpclient.URIException;

import com.idega.content.business.ContentConstants;
import com.idega.util.CoreConstants;
import com.idega.util.StringHandler;

public class CssScanner {
	
	private static Logger log = Logger.getLogger(CssScanner.class.getName());
	
	private BufferedReader readerBuffer = null;
	private StringBuffer resultBuffer = null;
	
	private String linkToTheme = null;
	
	private boolean needToReplace = false;
	
	private int openers = 0;
	private int closers = 0;
	
	private static final String COLOR_STRING = "color";
	private static final String COMMENT_BEGIN = "/*";
	private static final String COMMENT_END = "*/";
	private static final String OPENER = "{";
	private static final String CLOSER = "}";
	private static final String UTF_8_DECLARATION = "@charset \"UTF-8\";";
	
	public CssScanner(BufferedReader readerBuffer, String linkToTheme) {
		this.readerBuffer = readerBuffer;
		this.linkToTheme = linkToTheme;
		
		scanFile();
	}
	
	protected void scanFile() {
		if (readerBuffer == null) {
			return;
		}
		
		resultBuffer = new StringBuffer();
		String line;
		String changedLine = null;
		try {
			while ((line = readerBuffer.readLine()) != null) {
				changedLine = line;
				changedLine = scanLine(line);
				resultBuffer.append(changedLine).append(ThemesConstants.NEW_LINE);
				
				if (!line.equals(changedLine)) {	// If line was modified
					needToReplace = true;
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
	}
	
	private boolean canUseURL(String line) {
		boolean urlElementExists = true;
		
		String urlStart = "url";
		String[] urls = line.split(urlStart);
		if (urls == null || urls.length < 2) {
			return true;
		}
		
		File f = null;
		String urlValueInCss = null;
		String path = null;
		//	Zero element is not interesting...
		for (int i = 1; (i < urls.length && urlElementExists); i++) {
			urlValueInCss = urls[i];
			f = null;
			
			int start = urlValueInCss.indexOf(ContentConstants.BRACKET_OPENING);
			int end = urlValueInCss.lastIndexOf(ContentConstants.BRACKET_CLOSING);
			urlValueInCss = urlValueInCss.substring(start + 1, end);
			urlValueInCss = StringHandler.replace(urlValueInCss, "../", ThemesConstants.EMPTY);
			
			path = new StringBuffer(linkToTheme).append(urlValueInCss).toString();
			try {
				f = ThemesHelper.getInstance().getSlideService().getFile(path);
			} catch (URIException e) {
				e.printStackTrace();
			} catch (RemoteException e) {
				e.printStackTrace();
			}
			if (f == null) {
				urlElementExists = false;
			}
			else {
				try {
					urlElementExists = f.exists();
				} catch (Exception e) {
					e.printStackTrace();
					urlElementExists = false;
				}
			}
		}
		
		if (!urlElementExists) {
			log.log(Level.SEVERE, "File '" + path + "' does not exist in Theme's pack! Removing CSS expression: " + line);
		}
		return urlElementExists;
	}
	
	private String scanLine(String line) {
		if (line == null) {
			return ThemesConstants.EMPTY;
		}
		
		if (line.indexOf(UTF_8_DECLARATION) != -1) {
			line = StringHandler.replace(line, UTF_8_DECLARATION, CoreConstants.EMPTY);
			return line;
		}
		
		// Checking for incorrect hexidecimal values
		if (line.indexOf(CoreConstants.NUMBER_SIGN) != -1 && line.indexOf(COLOR_STRING) != -1) {
			String[] colorValue = line.split(CoreConstants.NUMBER_SIGN);
			if (colorValue == null) {
				return line;
			}
			if (colorValue.length != 2) {
				return line;
			}
			String color = colorValue[1];
			int index = StringHandler.getNotHexValueIndexInHexValue(color);
			String letterToReplace = null;
			while (index >= 0) {
				letterToReplace = CoreConstants.HEXIDECIMAL_LETTERS.get(ThemesHelper.getInstance().getRandomNumber(CoreConstants.HEXIDECIMAL_LETTERS.size()));
				if (index == color.length() - 1) {
					color = color.replace(color.substring(index), letterToReplace);
				}
				else {
					color = color.replace(color.substring(index, index + 1), letterToReplace);
				}
				index = StringHandler.getNotHexValueIndexInHexValue(color);
			}
			line = new StringBuffer(colorValue[0]).append(CoreConstants.NUMBER_SIGN).append(color).toString();
		}
		
		//	Checking URL stuff
		if (line.indexOf("url(") != -1) {
			if (canUseURL(line)) {
				return line;
			}
			else {
				return ThemesConstants.EMPTY;
			}
		}
		
		//	Checking comments
		if (line.indexOf(OPENER) == -1 && line.indexOf(CLOSER) == -1) {
			return line;
		}
		
		int commentBeginIndex = line.indexOf(COMMENT_BEGIN);
		int commentEndIndex = line.indexOf(COMMENT_END);
		int styleDefinitionBeginIndex = line.indexOf(OPENER);
		int styleDefinitionEndIndex = line.indexOf(CLOSER);
		if (commentBeginIndex != -1 && commentEndIndex != -1) {
			// CSS: /* www.multithemes.com */
			if (styleDefinitionBeginIndex == -1 && styleDefinitionEndIndex == -1) {
				return line;
			}
			
			//	CSS: /*comment*/body { or: body {/*comment*/
			if ((styleDefinitionBeginIndex > commentBeginIndex && styleDefinitionBeginIndex > commentEndIndex) || (styleDefinitionBeginIndex < commentBeginIndex && styleDefinitionBeginIndex < commentEndIndex)) {
				openers++;
				return line;
			}
			
			// CSS: /*comment*/} or }/*comment*/
			if ((styleDefinitionEndIndex > commentBeginIndex && styleDefinitionEndIndex > commentEndIndex) || (styleDefinitionEndIndex < commentBeginIndex && styleDefinitionEndIndex < commentEndIndex)) {
				return finishCssLine(line);
			}
		}
		else {
			// Lines of comment
			if (commentBeginIndex != -1 && commentEndIndex == -1) {
				if (styleDefinitionBeginIndex != -1 && styleDefinitionBeginIndex < commentBeginIndex) {
					openers++;
				}
				if (styleDefinitionEndIndex != -1 && styleDefinitionEndIndex < commentBeginIndex) {
					return finishCssLine(line);
				}
				
				return line;
			}

			if (commentBeginIndex == -1 && commentEndIndex != -1) {
				if (styleDefinitionBeginIndex != -1 && styleDefinitionBeginIndex > commentEndIndex) {
					openers++;
				}
				if (styleDefinitionEndIndex != -1 && styleDefinitionEndIndex > commentEndIndex) {
					return finishCssLine(line);
				}
				
				return line;
			}
		}
		
		if (line.indexOf(OPENER) != -1 && line.indexOf(CLOSER) != -1 && openers == closers) {
			return line;
		}
		
		if (line.indexOf(OPENER) != -1 && line.indexOf(CLOSER) == -1) {
			openers++;
			return line;
		}
		
		if (line.indexOf(OPENER) == -1 && line.indexOf(CLOSER) != -1) {
			return finishCssLine(line);
		}
		
		return line;
	}
	
	private String finishCssLine(String line) {
		closers++;
		if (closers != openers) {
			line = line.replace(CLOSER, ThemesConstants.EMPTY);
		}
		openers = 0;
		closers = 0;
		return line;
	}

	protected StringBuffer getResultBuffer() {
		return resultBuffer;
	}

	protected boolean isNeedToReplace() {
		return needToReplace;
	}

}
