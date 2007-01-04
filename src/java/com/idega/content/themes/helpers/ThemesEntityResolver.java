package com.idega.content.themes.helpers;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class ThemesEntityResolver implements EntityResolver {
	
	private static final String APPLE_PUBLIC_ID = "Apple Computer";
	private static final String APPLE_SYSTEM_ID = "PropertyList";

	public InputSource resolveEntity(String publicID, String systemID) throws SAXException, IOException {
		if (publicID == null || systemID == null) {
			return null;
		}
		if (publicID.indexOf(APPLE_PUBLIC_ID) != -1 && systemID.indexOf(APPLE_SYSTEM_ID) != -1) {
			return new InputSource(new ByteArrayInputStream("<?xml version='1.0' encoding='UTF-8'?>".getBytes()));
		}
		return null;
	}

}
