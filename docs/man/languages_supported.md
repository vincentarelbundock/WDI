
# languages_supported

List of supported languages

## Description

This prints two lists of languages, the fully supported ones and the
locally supported ones: \* the languages in the category "fully" will
return translated names and other info for all countries. \* the
languages in the category "partially" will return translated names and
other info only for the country they represent.

## Usage

<pre><code class='language-R'>languages_supported()
</code></pre>

## Details

For example, choosing "vi" (for Vietnamese) will translate "Vietnam" in
the dataset but other country names won’t be translated and will be
empty.

## Value

A list of fully and partially supported languages.
