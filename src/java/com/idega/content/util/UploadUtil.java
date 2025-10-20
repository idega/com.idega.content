package com.idega.content.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;

import com.idega.core.file.util.MimeTypeUtil;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.util.CoreConstants;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;

public class UploadUtil {

	private static final Logger LOGGER = Logger.getLogger(UploadUtil.class.getName());

	private static final UploadUtil instance = new UploadUtil();

	private static final Tika TIKA = new Tika();

	private UploadUtil() {}

	public static final UploadUtil getInstance() {
		return instance;
	}

	public List<String> getAllowedMediaTypes(IWMainApplicationSettings settings) {
		settings = settings == null ?
				IWMainApplication.getDefaultIWMainApplication().getSettings() :
				settings;
		return StringUtil.getValuesFromString(
				settings.getProperty(
						"content.allowed_media_types",
						ListUtil.convertListToCommaseparatedString(
								Arrays.asList(
										"image/png", "image/jpeg", "image/gif", "image/webp",
										"video/mp4", "video/webm", "video/ogg",
										"text/plain",
									    MimeTypeUtil.MIME_TYPE_PDF_1,
									    MimeTypeUtil.MIME_TYPE_PDF_2,
									    "application/vnd.openxmlformats-officedocument.wordprocessingml.document",		// .docx
									    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",			// .xlsx
									    "application/vnd.openxmlformats-officedocument.presentationml.presentation",	// .pptx
									    "application/msword",															// .doc
									    "application/vnd.ms-excel"														// .xls
								)
						)
				),
				CoreConstants.COMMA
		);
	}

	private List<Pattern> getDangerousPatterns() {
		try {
			IWMainApplicationSettings settings = IWMainApplication.getDefaultIWMainApplication().getSettings();
			List<String> patterns = StringUtil.getValuesFromString(
					settings.getProperty(
							"content.dangerous_patterns",
							ListUtil.convertListToCommaseparatedString(
									Arrays.asList(
											"<script.*?>",
											"</script>",
											"<iframe.*?>",
											"<img\\s+[^>]*src=['\"]?https?://",
											"<img\\s+[^>]*src=['\"]?http?://",
											"document\\.cookie",
											"fetch\\(",
											"XMLHttpRequest",
											"<marquee",
											"localStorage\\.getItem",
											"sessionStorage\\.getItem",
											"<script",
											"new\\s+Image\\(\\)",
											"(?i)<script.*?>",
											"(?i)</script>",
											"(?i)document\\.cookie",
											"(?i)fetch\\(",
											"(?i)XMLHttpRequest",
											"(?i)onerror\\s*=",
											"(?i)javascript:",
											"(?i)file:\\/\\/",
											"(?i)macro"
									)
							)
					),
					CoreConstants.COMMA
			);
			if (ListUtil.isEmpty(patterns)) {
				return null;
			}

			List<Pattern> dangerousPatterns = new ArrayList<>();
			for (String pattern: patterns) {
				if (StringUtil.isEmpty(pattern)) {
					continue;
				}

				dangerousPatterns.add(Pattern.compile(pattern, Pattern.CASE_INSENSITIVE));
			}
			return dangerousPatterns;
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error getting dangerous patterns", e);
		}
		return null;
	}

	public boolean isContentSuspicious(byte[] content) {
		if (content == null) {
			return false;
		}

		try {
			String extractedText = null;
			InputStream input = null;
	        try {
	        	input = new ByteArrayInputStream(content);
	            extractedText = TIKA.parseToString(input);
	        } catch (IOException | TikaException e) {
	        } finally {
	        	IOUtil.close(input);
	        }

	        if (StringUtil.isEmpty(extractedText)) {
	        	extractedText = new String(content, StandardCharsets.UTF_8);
	        }

	        if (StringUtil.isEmpty(extractedText)) {
	        	return true;
	        }

	        List<Pattern> patterns = getDangerousPatterns();
	        if (ListUtil.isEmpty(patterns)) {
	        	return false;
	        }

	        for (Pattern pattern: patterns) {
	            if (pattern.matcher(extractedText).find()) {
	            	LOGGER.warning("Suspicious content detected: " + pattern);
	                return true;
	            }
	        }
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error while checink if content is suspicious", e);
		}
        return false;
	}

	public String getSanitized(String content) {
		if (StringUtil.isEmpty(content)) {
			return content;
		}

		String sanitationPattern = IWMainApplication.getDefaultIWMainApplication().getSettings().getProperty("content.sanitation_pattern", "[^a-zA-Z0-9._/\\\\-]");
		if (StringUtil.isEmpty(sanitationPattern)) {
			return content;
		}

		return content.replaceAll(sanitationPattern, CoreConstants.UNDER);
	}

	public String getMimeType(byte[] bytes) {
		if (bytes == null) {
			return null;
		}

		return TIKA.detect(bytes);
	}

}