package com.idega.content.themes.helpers.business;

import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import bsh.Interpreter;

import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;

/**
 * @author <a href="mailto:valdas@idega.com">Valdas Žemaitis</a>
 * @version $Revision: 1.1 $
 *
 * Calculates color value (hex value) from given expression
 *
 * Last modified: $Date: 2008/06/17 15:13:43 $ by $Author: valdas $
 */

@Service
@Scope("singleton")
public class ColourExpressionCalculator {
	
	private static final Logger logger = Logger.getLogger(ColourExpressionCalculator.class.getName());
	
	private static final String[] _SEARCH_TERMS = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "A", "B", "C", "D", "E", "F"};
	private static final List<String> SEARCH_TERMS = Collections.unmodifiableList(Arrays.asList(_SEARCH_TERMS));
	
	private static final String[] _COLOUR_MATH_OPERATIONS = {CoreConstants.PLUS, CoreConstants.MINUS, CoreConstants.STAR};
	private static final List<String> COLOUR_MATH_OPERATIONS = Collections.unmodifiableList(Arrays.asList(_COLOUR_MATH_OPERATIONS));
	
	private static final String DEFAULT_COLOUR = "#ffffff";
	
	private static final String RED_COLOUR_START = "r(";
	private static final String GREEN_COLOUR_START = "g(";
	private static final String BLUE_COLOUR_START = "b(";
	
	private static final Map<String, Integer> HEX_TO_DEC_NUMBERS_CACHE = new HashMap<String, Integer>();
	
	private ThemesHelper helper = null;
	private Interpreter mathInterpreter = null;
	
	public ColourExpressionCalculator() {
		mathInterpreter = new bsh.Interpreter();
		
		helper = ThemesHelper.getInstance();
	}

	protected boolean setValuesToColourFiles(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		if (!theme.hasColourFiles()) {
			return true;
		}
		
		List<String> colourFiles = theme.getColourFiles();
		IWSlideService slide = helper.getSlideService();
		if (slide == null) {
			return false;
		}
		
		List<String> keys = theme.getStyleVariablesKeys();
		
		String file = null;
		String webRoot = helper.getFullWebRoot();
		String sourceLink = null;
		InputStream stream = null;
		String key = null;
		for (int i = 0; i < colourFiles.size(); i++) {
			file = colourFiles.get(i);
			
			sourceLink = new StringBuffer(webRoot).append(theme.getLinkToBase()).append(helper.getThemeColourFileName(theme, null, file, true)).toString();
			stream = helper.getInputStream(sourceLink);
			if (stream == null) {
				return false;
			}
			
			String content = null;
			try {
				content = StringHandler.getContentFromInputStream(stream);
			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				helper.closeInputStream(stream);
			}
			if (content == null) {
				return false;
			}
			
			for (int j = 0; (j < keys.size() && content != null); j++) {
				key = keys.get(j);
				content = replaceColourFileContent(content, key, theme.getStyleVariableValue(key));
			}
			
			content = checkIfAllVariablesReplaced(content, theme);
			
			if (content == null) {
				return false;
			}
			
			try {
				if (!(slide.uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBase(), file, content, CoreConstants.CONTENT_TYPE_TEXT_CSS, true))) {
					return false;
				}
			} catch (RemoteException e) {
				e.printStackTrace();
				return false;
			}
		}
		
		return true;
	}
	
	private String checkIfAllVariablesReplaced(String content, Theme theme) {
		if (content == null || theme == null) {
			return null;
		}
		
		int start = -1;
		int end = -1;
		List<String> searchTerm = ListUtil.convertStringArrayToList(new String[] {CoreConstants.PERCENT});
		String cssExpression = null;
		String fixedCssExpression = null;
		String variable = null;
		String styleValue = null;
		ThemeStyleGroupMember colourVariation = null;
		while (content.indexOf(CoreConstants.PERCENT) != -1) {
			cssExpression = null;
			fixedCssExpression = null;
			variable = null;
			styleValue = null;
			colourVariation = null;
			
			start = getStartIndexForCssVariable(content, CoreConstants.PERCENT, CoreConstants.PERCENT);
			end = getEndIndexForCssVariable(content, CoreConstants.PERCENT, searchTerm, true, true);
			
			if (!(canSubstring(content, start, end))) {
				return content;
			}

			cssExpression = content.substring(start, end);
			variable = StringHandler.remove(cssExpression, CoreConstants.PERCENT);
			styleValue = theme.getStyleVariableValue(variable);
			if (styleValue == null) {
				try {
					colourVariation = helper.getThemeChanger().getColorGroupMember(theme, variable);
				} catch (Exception e) {
					logger.log(Level.SEVERE, "Error while getting colour variation", e);
				}
				if (colourVariation != null) {
					styleValue = colourVariation.getColour();
				}
			}
			if (styleValue == null) {
				styleValue = DEFAULT_COLOUR;
			}
			
			fixedCssExpression = computeCssValue(cssExpression, variable, styleValue);
			content = StringHandler.replace(content, cssExpression, fixedCssExpression);
		}
		
		return content;
	}
	
	private String replaceColourFileContent(String content, String variable, String value) {
		if (content == null) {
			return null;
		}
		if (variable == null || value == null) {
			return null;
		}
		
		int start = -1;
		int end = -1;
		String originalValue = null;
		String computedCSSValue = null;
		List<String> searchTerm = new ArrayList<String>();
		searchTerm.add(CoreConstants.PERCENT);
		while (content.indexOf(variable) != -1) {
			start = getStartIndexForCssVariable(content, variable, CoreConstants.PERCENT);
			end = getEndIndexForCssVariable(content, variable, searchTerm, true, true);
			if (!canSubstring(content, start, end)) {
				logger.log(Level.WARNING, "Can not extract CSS expression '"+variable+"' because of invalid indexes: start index: " + start + ", end index: " + end +
						". Using default colour: '"+DEFAULT_COLOUR+"'");
				computedCSSValue = DEFAULT_COLOUR;	//	Error
			}
			
			originalValue = content.substring(start, end);
			computedCSSValue = computeCssValue(originalValue, variable, value);
			if (computedCSSValue == null) {
				logger.log(Level.WARNING, "Error occured computed CSS value for variable '"+variable+"', using default colour ('"+DEFAULT_COLOUR+"') for this variable.");
				computedCSSValue = DEFAULT_COLOUR;	//	Error
			}
			else if (computedCSSValue.length() != 4 && computedCSSValue.length() != 7) {
				int requiredLength = computedCSSValue.length() > 4 ? 7 : 4;
				while (computedCSSValue.length() < requiredLength) {
					computedCSSValue = computedCSSValue.replaceFirst(CoreConstants.NUMBER_SIGN, String.valueOf(CoreConstants.NUMBER_SIGN + 0));
				}
			}
			
			content = StringHandler.replace(content, originalValue, computedCSSValue);
		}
		
		return content;
	}
	
	private String computeCssValue(String colourExpression, String variable, String value) {
		if (colourExpression.indexOf(CoreConstants.PLUS) == -1 && colourExpression.indexOf(CoreConstants.MINUS) == -1 &&
				colourExpression.indexOf(CoreConstants.STAR) == -1) {
			//	No math expression
			return value;
		}
		
		String finalHexExpression = null;
		
		colourExpression = colourExpression.toLowerCase();
		colourExpression = StringHandler.replace(colourExpression, CoreConstants.PERCENT, CoreConstants.EMPTY);	//	Removing '%'
		colourExpression = StringHandler.replace(colourExpression, CoreConstants.SPACE, CoreConstants.EMPTY);	//	Removing white spaces
		colourExpression = StringHandler.replace(colourExpression, variable, value);							//	Replacing variable with value
		
		if (!isWithoutRGBExpression(colourExpression)) {
			colourExpression = getComputedHeximalValueByRGB(colourExpression);									//	RGB expression(s) will be replaced with hex number(s)
		}
			
		finalHexExpression = getComputedHeximalValueByCSSExpression(colourExpression);							//	-, +, * expression(s)
		
		return finalHexExpression == null ? value : finalHexExpression;
	}
	
	private String getComputedHeximalValueByRGB(String colourExpression) {
		if (colourExpression == null) {
			return null;
		}
		
		if (isWithoutRGBExpression(colourExpression)) {
			return colourExpression;
		}
		
		boolean roundResult = isNeedRoundResult(colourExpression);
		
		String redColourValue = extractRGBValue(colourExpression.substring(colourExpression.indexOf(RED_COLOUR_START) + RED_COLOUR_START.length()));
		if (redColourValue == null) {
			return null;
		}
		String greenColourValue = extractRGBValue(colourExpression.substring(colourExpression.indexOf(GREEN_COLOUR_START) + GREEN_COLOUR_START.length()));
		if (greenColourValue == null) {
			return null;
		}
		String blueColourValue = extractRGBValue(colourExpression.substring(colourExpression.indexOf(BLUE_COLOUR_START) + BLUE_COLOUR_START.length()));
		if (blueColourValue == null) {
			return null;
		}
		
		String MAX_VALUE = String.valueOf(255);
		String operation = CoreConstants.STAR;
		List<Integer> computedHexPartsInDecimals = new ArrayList<Integer>(3);
		makeMathOperation(MAX_VALUE, operation, redColourValue, computedHexPartsInDecimals, roundResult);
		makeMathOperation(MAX_VALUE, operation, greenColourValue, computedHexPartsInDecimals, roundResult);
		makeMathOperation(MAX_VALUE, operation, blueColourValue, computedHexPartsInDecimals, roundResult);
		
		String rgbReplacement = getConvertedDecimalsToHex(computedHexPartsInDecimals);
		if (rgbReplacement == null) {
			return null;
		}
		
		String fullRGBExpression = new StringBuffer(RED_COLOUR_START).append(redColourValue).append(ContentConstants.BRACKET_CLOSING).append(GREEN_COLOUR_START)
									.append(greenColourValue).append(ContentConstants.BRACKET_CLOSING).append(BLUE_COLOUR_START).append(blueColourValue)
									.append(ContentConstants.BRACKET_CLOSING).toString();
		colourExpression = StringHandler.replace(colourExpression, fullRGBExpression, rgbReplacement);
		return getComputedHeximalValueByRGB(colourExpression);
	}
	
	private boolean isWithoutRGBExpression(String expression) {
		return expression.indexOf(RED_COLOUR_START) == -1 && expression.indexOf(GREEN_COLOUR_START) == -1 && expression.indexOf(BLUE_COLOUR_START) == -1;
	}
	
	private String extractRGBValue(String value) {
		int index = 0;
		String currentSymbol = value.substring(index, index + 1);
		while ((StringHandler.isNaturalNumber(currentSymbol) || CoreConstants.DOT.equals(currentSymbol)) && index + 1 < value.length()) {
			index++;
			currentSymbol = value.substring(index, index + 1);
		}
		
		String extractedValue = value.substring(0, index);
		return extractedValue;
	}
	
	private String getComputedHeximalValueByCSSExpression(String expression) {
		//	Firstly making multiplication operations
		expression = makeMultiplication(expression);
		if (expression == null) {
			return null;
		}
		
		String variable = getHexValueFromExpression(expression);
		if (variable == null) {
			return null;
		}
		
		expression = StringHandler.replace(expression, variable, CoreConstants.EMPTY);
		
		String operand = null;
		List<String> operands = new ArrayList<String>();
		while (expression.indexOf(CoreConstants.NUMBER_SIGN) != -1) {
			operand = getHexValueFromExpression(expression);
			if (operand == null) {
				return null;
			}
			
			operands.add(operand);
			expression = StringHandler.replace(expression, operand, CoreConstants.EMPTY);
		}
		
		if (operands.isEmpty()) {
			return variable;
		}
		if (expression == null || expression.equals(CoreConstants.EMPTY)) {
			return null;
		}
		expression = expression.replaceAll(CoreConstants.SPACE, CoreConstants.EMPTY);
		if (expression.equals(CoreConstants.EMPTY) || expression.length() != operands.size()) {
			return null;
		}
		
		//	Now left only operations: -, +
		int index = 0;
		String operation = null;
		String computedCSSValue = variable;
		while (expression.length() > 0) {
			operation = expression.substring(0, 1);
			if (operation.equals(CoreConstants.MINUS) || operation.equals(CoreConstants.PLUS)) {
				computedCSSValue = getExecutedColourOperation(computedCSSValue, operation, operands.get(index));
				
				if (computedCSSValue == null) {
					return null;
				}
			}
			
			expression = expression.substring(1);
			index++;
		}
		
		return computedCSSValue;
	}
	
	private boolean isNeedRoundResult(String expression) {
		if (expression == null) {
			return false;
		}
		
		return expression.indexOf(CoreConstants.MINUS) != -1;
	}
	
	private String makeMultiplication(String cssColourExpression) {
		if (cssColourExpression == null) {
			return null;
		}
		if (cssColourExpression.indexOf(CoreConstants.STAR) == -1) {
			return cssColourExpression;
		}
		
		String expression = cssColourExpression.replaceAll(CoreConstants.SPACE, CoreConstants.EMPTY);
		if (expression.startsWith(CoreConstants.STAR) || expression.endsWith(CoreConstants.STAR)) {
			return null;
		}
		
		boolean roundResult = isNeedRoundResult(expression);
		int operationIndex = expression.indexOf(CoreConstants.STAR);
		
		String leftOperand = null;
		try {
			String leftPart = expression.substring(0, operationIndex);
			int indexFromTheEnd = 1;
			int leftPartLength = leftPart.length();
			String currentSymbol = leftPart.substring(leftPartLength - indexFromTheEnd);
			while ((StringHandler.isNaturalNumber(currentSymbol) || CoreConstants.DOT.equals(currentSymbol) || SEARCH_TERMS.contains(currentSymbol)) &&
					leftPartLength - indexFromTheEnd > 0) {
				indexFromTheEnd++;
				currentSymbol = leftPart.substring(leftPartLength - indexFromTheEnd, leftPartLength - indexFromTheEnd + 1);
			}
			leftOperand = leftPart.substring(leftPartLength - indexFromTheEnd);
		} catch(Exception e) {
			logger.log(Level.SEVERE, "Error getting left operand for multiplication operation", e);
			return null;
		}
		if (leftOperand == null) {
			return null;
		}
		
		String rightOperand = null;
		try {
			int index = 0;
			String rightPart = expression.substring(operationIndex + 1);
			String currentSymbol = rightPart.substring(index, index + 1);
			boolean firstOccuranceOfNumberSign = CoreConstants.NUMBER_SIGN.equals(currentSymbol);
			while ((StringHandler.isNaturalNumber(currentSymbol) || CoreConstants.DOT.equals(currentSymbol) || firstOccuranceOfNumberSign ||
					SEARCH_TERMS.contains(currentSymbol)) && !COLOUR_MATH_OPERATIONS.contains(currentSymbol) && index + 1 < rightPart.length()) {
				
				firstOccuranceOfNumberSign = false;
				index++;
				currentSymbol = rightPart.substring(index, index + 1);
			}
			int endIndex = COLOUR_MATH_OPERATIONS.contains(currentSymbol) ? index : index + 1;
			rightOperand = rightPart.substring(0, endIndex);
		} catch(Exception e) {
			logger.log(Level.SEVERE, "Error getting right operand for multiplication operation", e);
			return null;
		}
		if (rightOperand == null) {
			return null;
		}
		
		int startIndex = cssColourExpression.lastIndexOf(leftOperand);
		int endIndex = cssColourExpression.lastIndexOf(rightOperand) + rightOperand.length();
		if (!canSubstring(cssColourExpression, startIndex, endIndex)) {
			return null;
		}
		String expressionToReplace = cssColourExpression.substring(startIndex, endIndex);
		if (expressionToReplace == null) {
			return null;
		}
		
		String computedValue = null;
		if (leftOperand.startsWith(CoreConstants.NUMBER_SIGN) && rightOperand.startsWith(CoreConstants.NUMBER_SIGN)) {
			//	Both operands - hex numbers
			computedValue = getExecutedColourOperation(leftOperand, CoreConstants.STAR, rightOperand);
		}
		else {
			if (!leftOperand.startsWith(CoreConstants.NUMBER_SIGN) && !rightOperand.startsWith(CoreConstants.NUMBER_SIGN)) {
				//	Both operand - decimal numbers
				Integer computedValueWithoutHexNumber = getComputedValue(expressionToReplace, roundResult);
				if (computedValueWithoutHexNumber == null) {
					return null;
				}
				computedValue = String.valueOf(computedValueWithoutHexNumber);
			}
			else {
				//	One of operands is hex and another - decimal
				String[] splittedHexOperand = null;
				String decimalOperand = null;
				if (leftOperand.startsWith(CoreConstants.NUMBER_SIGN)) {
					splittedHexOperand = getHexValueSplitted(leftOperand);
					decimalOperand = rightOperand;
				}
				else {
					splittedHexOperand = getHexValueSplitted(rightOperand);
					decimalOperand = leftOperand;
				}
				if (splittedHexOperand == null || decimalOperand == null) {
					return null;
				}
				
				List<Integer> hexPartsInDecimals = new ArrayList<Integer>();
				for (String hexPart: splittedHexOperand) {
					makeMathOperation(hexPart, CoreConstants.STAR, decimalOperand, hexPartsInDecimals, roundResult);
				}
				computedValue = getConvertedDecimalsToHex(hexPartsInDecimals);
			}
		}
		
		if (computedValue == null) {
			return null;
		}
		cssColourExpression = StringHandler.replace(cssColourExpression, expressionToReplace, computedValue);
		return makeMultiplication(cssColourExpression);
	}
	
	private String getExecutedColourOperation(String operand1, String operation, String operand2) {
		String[] splitted1 = getHexValueSplitted(operand1);
		String[] splitted2 = getHexValueSplitted(operand2);
	
		if (splitted1 == null || splitted2 == null) {
			return null;
		}
		
		List<Integer> hexPartsInDecimals = new ArrayList<Integer>();
		for (int i = 0; i <splitted1.length; i++) {
			makeMathOperation(splitted1[i], operation, splitted2[i], hexPartsInDecimals, false);
		}

		return getConvertedDecimalsToHex(hexPartsInDecimals);
	}
	
	private void makeMathOperation(String operand1, String operation, String operand2, List<Integer> computedHexPartsInDecimals, boolean roundResult) {
		Integer resultInDecimals = getComputedValue(getMathOperation(operand1, operation, operand2), roundResult);
		if (resultInDecimals == null) {
			return;
		}
		computedHexPartsInDecimals.add(resultInDecimals);
	}
	
	private Integer getComputedValue(String mathOperation, boolean roundResult) {
		if (mathOperation == null) {
			return null;
		}
		
		Object result = null;
		String cacheKey = roundResult ? new StringBuffer("rounded_").append(mathOperation).toString() : mathOperation;
		Integer resultInDecimals = HEX_TO_DEC_NUMBERS_CACHE.get(cacheKey);
		if (resultInDecimals == null) {
			try {
				result = mathInterpreter.eval(mathOperation);
			} catch(Exception e) {
				logger.log(Level.SEVERE, "Error while computing CSS colour value: " + mathOperation, e);
				return null;
			}
			if (result == null) {
				logger.log(Level.SEVERE, "Error while computing CSS colour value: " + mathOperation);
				return null;
			}
			
			if (result instanceof Double) {
				Double resultAsDouble = (Double) result;
				if (roundResult) {
					resultInDecimals = Long.valueOf(Math.round(resultAsDouble)).intValue();
				}
				else {
					resultInDecimals = resultAsDouble.intValue();
				}
			}
			else if (result instanceof Integer) {
				resultInDecimals = (Integer) result;
			}
			if (resultInDecimals == null) {
				return null;
			}
			
			HEX_TO_DEC_NUMBERS_CACHE.put(cacheKey, resultInDecimals);
		}
		
		return resultInDecimals;
	}
	
	private String getConvertedDecimalsToHex(List<Integer> hexPartsInDecimals) {
		if (hexPartsInDecimals == null || hexPartsInDecimals.isEmpty()) {
			return null;
		}
		
		String hexPartInString = null;
		StringBuffer computedHexValue = new StringBuffer(CoreConstants.NUMBER_SIGN);
		for (Integer hexPart: hexPartsInDecimals) {
			if (hexPart < 0) {
				hexPart = 0;
			}
			if (hexPart > 255) {
				hexPart = 255;
			}
			
			hexPartInString = null;
			try {
				hexPartInString = Integer.toHexString(hexPart);
			} catch(NumberFormatException e) {
				logger.log(Level.SEVERE, "Error while converting decimal to hex: " + hexPart, e);
				return null;
			}
			if (hexPartInString == null) {
				return null;
			}
			if (hexPartInString.length() == 1) {
				hexPartInString = new StringBuffer("0").append(hexPartInString).toString();
			}
			
			computedHexValue.append(hexPartInString);
		}
		
		return computedHexValue.toString();
	}
	
	private String getMathOperation(String operand1, String operation, String operand2) {
		StringBuffer mathOperation = null;
		try {
			mathOperation = new StringBuffer().append(operand1.startsWith(CoreConstants.NUMBER_SIGN) ? Integer.decode(operand1) : operand1).append(operation);
			mathOperation.append(operand2.startsWith(CoreConstants.NUMBER_SIGN) ? Integer.decode(operand2) : operand2);
		} catch(NumberFormatException e) {
			logger.log(Level.SEVERE, "Error while converting hex value to decimal", e);
			return null;
		}
		
		return mathOperation == null ? null : mathOperation.toString();
	}
	
	private String[] getHexValueSplitted(String hexValue) {
		if (hexValue.startsWith(CoreConstants.NUMBER_SIGN)) {
			hexValue = StringHandler.replace(hexValue, CoreConstants.NUMBER_SIGN, CoreConstants.EMPTY);
		}
		
		hexValue = hexValue.trim();
		
		if (hexValue.length() != 3 && hexValue.length() != 6) {
			logger.log(Level.WARNING, "Invalid length (must be 3 or 6) for hex value: " + hexValue);
			return null;
		}
		
		String[] hexParts = new String[3];
		int beginIndex = 0;
		int stepSize = hexValue.length() == 3 ? 1 : 2;
		int endIndex = stepSize;
		for (int i = 0; i < hexParts.length; i++) {
			if (!(canSubstring(hexValue, beginIndex, endIndex))) {
				logger.log(Level.WARNING, "Got invalid indexes (begin: " + beginIndex + ", end: " + endIndex + ") while splitting hex value: " + hexValue);
				return null;
			}
			
			hexParts[i] = new StringBuilder(CoreConstants.NUMBER_SIGN).append(hexValue.substring(beginIndex, endIndex)).toString();
			beginIndex += stepSize;
			endIndex += stepSize;
		}
		
		return hexParts;
	}
	
	private String getHexValueFromExpression(String expression) {
		int start = getStartIndexForCssVariable(expression, CoreConstants.NUMBER_SIGN, CoreConstants.NUMBER_SIGN);
		int end = getEndIndexForCssVariable(expression, CoreConstants.NUMBER_SIGN, SEARCH_TERMS, false, false);
		
		if (!(canSubstring(expression, start, end))) {
			return null;	//	Error
		}
			
		return expression.substring(start, end);
	}
	
	private boolean canSubstring(String content, int start, int end) {
		if (content == null || start == -1 || end == -1) {
			return false;
		}
		
		if (start < 0 || end <= start || end > content.length()) {
			return false;
		}
		
		return true;
	}
	
	private int getEndIndexForCssVariable(String content, String key, List<String> searchTerms, boolean checkIfContains, boolean increaseIndex) {
		int index = content.indexOf(key);
		if (index == -1) {
			return -1;
		}
		
		index += key.length();
		
		boolean foundPercentMarkAtTheEnd = false;
		while (!foundPercentMarkAtTheEnd && index < content.length()) {
			if (checkIfContains) {
				if (searchTerms.contains(content.substring(index, index + 1))) {
					foundPercentMarkAtTheEnd = true;
				}
				else {
					index++;
				}
			}
			else {
				if (!(searchTerms.contains(content.substring(index, index + 1)))) {
					foundPercentMarkAtTheEnd = true;
				}
				else {
					index++;
				}
			}
		}
		
		if (increaseIndex) {
			if (index + 1 < content.length()) {
				index++;
			}
		}
		return index;
	}
	
	private int getStartIndexForCssVariable(String content, String key, String searchTerm) {
		int index = content.indexOf(key);
		if (index == -1) {
			return -1;
		}
		
		boolean foundPercentMarkAtTheBegin = false;
		while (!foundPercentMarkAtTheBegin && index > 0) {
			if (content.substring(index).startsWith(searchTerm)) {
				foundPercentMarkAtTheBegin = true;
			}
			else {
				index--;
			}
		}
		
		return index;
	}
}
