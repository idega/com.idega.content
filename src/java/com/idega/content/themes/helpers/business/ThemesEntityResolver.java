package com.idega.content.themes.helpers.business;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class ThemesEntityResolver implements EntityResolver {

	public InputSource resolveEntity(String publicID, String systemID) throws SAXException, IOException {
		if (publicID == null || systemID == null) {
			return null;
		}
		for (int i = 0; i < ThemesConstants.DOCUMENT_PUBLIC_IDS.size(); i++) {
			if (publicID.indexOf(ThemesConstants.DOCUMENT_PUBLIC_IDS.get(i)) != -1 &&
					systemID.indexOf(ThemesConstants.DOCUMENT_SYSTEM_IDS.get(i)) != -1) {
				return new InputSource(new ByteArrayInputStream(ThemesConstants.DOCUMENT_HEADER.getBytes()));
			}
		}
		return null;
	}

}
