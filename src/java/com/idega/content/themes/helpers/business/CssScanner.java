package com.idega.content.themes.helpers.business;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.content.business.ContentConstants;
import com.idega.servlet.filter.IWBundleResourceFilter;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.resources.ResourceScanner;

@Scope(BeanDefinition.SCOPE_PROTOTYPE)
@Service(CssScanner.SPRING_BEAN_IDENTIFIER)
public class CssScanner implements ResourceScanner {

	private static final long serialVersionUID = -694282023660051365L;
	private static Logger LOGGER = Logger.getLogger(CssScanner.class.getName());

	public static final String SPRING_BEAN_IDENTIFIER = "cssScanner";

	private StringBuffer resultBuffer = null;

	private String linkToTheme = null;

	private boolean needToReplace = false;

	private int openers = 0;
	private int closers = 0;

	private static final String COLOR_STRING = "color";
	private static final String COMMENT_BEGIN = "/*";
	private static final String COMMENT_END = "*/";
	private static final String DIRECTORY_LEVEL_UP = "../";
	private static final String OPENER = "{";
	private static final String CLOSER = "}";
	private static final String UTF_8_DECLARATION = "@charset \"UTF-8\";";

	@Autowired
	private ThemesHelper themesHelper;

	@Override
	public void scanFile(List<String> fileLines) {
		openers = 0;
		closers = 0;
		needToReplace = false;
		resultBuffer = new StringBuffer();

		String changedLine = null;
		for (String line: fileLines) {
			changedLine = line;
			changedLine = scanLine(line);
			resultBuffer.append(changedLine).append(ThemesConstants.NEW_LINE);

			if (!line.equals(changedLine)) {	// If line was modified
				needToReplace = true;
			}
		}
	}

	@Override
	public void setLinkToTheme(String linkToTheme) {
		this.linkToTheme = linkToTheme;
	}

	private boolean canUseURL(String line) {
		String urlExpressionStart = "url(";

		int start = line.indexOf(urlExpressionStart);
		int end = line.lastIndexOf(ContentConstants.BRACKET_CLOSING);
		if (start < 0 || end < 0) {
			return false;
		}

		String urlExpression = line.substring(start + urlExpressionStart.length(), end);
		urlExpression = StringHandler.replace(urlExpression, DIRECTORY_LEVEL_UP, ThemesConstants.EMPTY);
		urlExpression = StringHandler.replace(urlExpression, "'", CoreConstants.EMPTY);
		urlExpression = StringHandler.replace(urlExpression, "\"", CoreConstants.EMPTY);
		String path = new StringBuffer(linkToTheme).append(urlExpression).toString();

		try {
			return getThemesHelper().getRepositoryService().getExistence(path);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error checking if file '" + path + "' exists", e);
		}
		return false;
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
				letterToReplace = CoreConstants.HEXIDECIMAL_LETTERS.get(getThemesHelper().getRandomNumber(CoreConstants.HEXIDECIMAL_LETTERS.size()));
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
				LOGGER.log(Level.WARNING, "Removing CSS expression: '" + line + "' because this object was not found in theme's pack!");
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
			if ((styleDefinitionBeginIndex > commentBeginIndex && styleDefinitionBeginIndex > commentEndIndex) ||
					(styleDefinitionBeginIndex < commentBeginIndex && styleDefinitionBeginIndex < commentEndIndex)) {
				openers++;
				return line;
			}

			// CSS: /*comment*/} or }/*comment*/
			if ((styleDefinitionEndIndex > commentBeginIndex && styleDefinitionEndIndex > commentEndIndex) ||
					(styleDefinitionEndIndex < commentBeginIndex && styleDefinitionEndIndex < commentEndIndex)) {
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

	@Override
	public StringBuffer getResultBuffer() {
		return resultBuffer;
	}

	@Override
	public boolean isNeedToReplace() {
		return needToReplace;
	}

	@Override
	public String getParsedContent(List<String> contentLines, String fileUri) {
		if (ListUtil.isEmpty(contentLines) || fileUri == null) {
			return null;
		}

		String parsedLine = null;
		StringBuilder parsedContent = new StringBuilder();
		for (String line: contentLines) {
			parsedLine = getParsedLine(line, fileUri);
			parsedContent.append("\n").append(parsedLine);
		}

		return parsedContent.toString();
	}

	private String getParsedLine(String line, String fileUri) {
		String urlExpressionStart = "url(";
		if (line.indexOf(urlExpressionStart) == -1) {
			return line;
		}
		if (line.indexOf(IWBundleResourceFilter.BUNDLES_STANDARD_DIR) != -1) {
			return line;
		}

		line = line.replaceAll(CoreConstants.QOUTE_SINGLE_MARK, CoreConstants.EMPTY);
		line = line.replaceAll(CoreConstants.QOUTE_MARK, CoreConstants.EMPTY);

		int startIndex = line.indexOf(urlExpressionStart) + urlExpressionStart.length();
		int endIndex = line.indexOf(")");
		String urlExpression = null;
		if (startIndex != -1 && endIndex != -1 && startIndex <= endIndex) {
			urlExpression = line.substring(startIndex, endIndex);

		}
		if (urlExpression == null) {
			return line;
		}

		String originalExpression = urlExpression;
		String urlReplacement = null;

		int levelsUp = 0;
		if (urlExpression.startsWith(DIRECTORY_LEVEL_UP)) {
			while (urlExpression.indexOf(DIRECTORY_LEVEL_UP) != -1) {
				urlExpression = urlExpression.replaceFirst(DIRECTORY_LEVEL_UP, CoreConstants.EMPTY);
				levelsUp++;
			}
		}

		if (levelsUp == 0) {
			if (!urlExpression.startsWith(fileUri)) {
				urlReplacement = new StringBuilder(fileUri).append(urlExpression).toString();
			}
		}
		else {
			String[] resourceParts = fileUri.split(CoreConstants.SLASH);
			if (ArrayUtil.isEmpty(resourceParts)) {
				return line;
			}

			StringBuilder newUrlExpression = new StringBuilder();
			for (int i = 0; i < (resourceParts.length - levelsUp); i++) {
				if (!StringUtil.isEmpty(resourceParts[i])) {
					newUrlExpression.append(CoreConstants.SLASH).append(resourceParts[i]);
				}
			}

			urlReplacement = newUrlExpression.append(CoreConstants.SLASH).append(urlExpression).toString();
		}

		if (StringUtil.isEmpty(urlReplacement)) {
			return line;
		}

		line = line.replace(originalExpression, urlReplacement);
		return line;
	}

	public ThemesHelper getThemesHelper() {
		return themesHelper;
	}

	public void setThemesHelper(ThemesHelper themesHelper) {
		this.themesHelper = themesHelper;
	}

}