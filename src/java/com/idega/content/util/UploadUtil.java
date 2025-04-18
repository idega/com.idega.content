package com.idega.content.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
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

	private static final Pattern[] DANGEROUS_PATTERNS = new Pattern[] {
			Pattern.compile("<script.*?>", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("</script>", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("<iframe.*?>", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("<img\\s+[^>]*src=['\"]?https?://", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("<img\\s+[^>]*src=['\"]?http?://", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("document\\.cookie", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("fetch\\(", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("XMLHttpRequest", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("<marquee", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("localStorage\\.getItem", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("sessionStorage\\.getItem", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("<script", Pattern.CASE_INSENSITIVE),
	        Pattern.compile("new\\s+Image\\(\\)", Pattern.CASE_INSENSITIVE),

	        Pattern.compile("(?i)<script.*?>"),
	        Pattern.compile("(?i)</script>"),
	        Pattern.compile("(?i)document\\.cookie"),
	        Pattern.compile("(?i)fetch\\("),
	        Pattern.compile("(?i)XMLHttpRequest"),
	        Pattern.compile("(?i)onerror\\s*="),
	        Pattern.compile("(?i)javascript:"),
	        Pattern.compile("(?i)file:\\/\\/"),
	        Pattern.compile("(?i)macro", Pattern.CASE_INSENSITIVE)
	};

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

	public boolean isContentSuspicious(byte[] content) {
		if (content == null) {
			return false;
		}

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

        for (Pattern pattern: DANGEROUS_PATTERNS) {
            if (pattern.matcher(extractedText).find()) {
            	LOGGER.warning("Suspicious content detected: " + pattern);
                return true;
            }
        }
        return false;
	}

	public String getSanitized(String content) {
		if (StringUtil.isEmpty(content)) {
			return content;
		}

		return content.replaceAll("[^a-zA-Z0-9._-]", CoreConstants.UNDER);
	}

	public String getMimeType(byte[] bytes) {
		if (bytes == null) {
			return null;
		}

		return TIKA.detect(bytes);
	}

}