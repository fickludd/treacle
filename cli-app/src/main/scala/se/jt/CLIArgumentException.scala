package se.jt

import java.io.IOException

class CLIArgumentException(
		message:String,
		cause:Throwable = null
) extends IOException(message, cause) {}
