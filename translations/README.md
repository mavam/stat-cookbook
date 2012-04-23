This directory contains the translations of the textual elements of the
cookbook.

Maintainers
===========

Each language has one (or more) maintainers responsible for the quality and
life cycle of the corresponding dictionary. Feel free to contact the
maintainer(s) regarding any issues with the translation. The preferred way of
doing so is by [opening a new issue][new issue]. If you would like to be the
maintainer for a new language, please contact me and CC existing maintainers
for that language (if there are any).

Adding a New Language
=====================

To facilitate the translation process, I use the [translator][translator] LaTeX
package. Each language is a separate *dictionary* file, named in the format
`probstat-LANGUAGE.dict`. In order to add a new dictionary for an unsupported
language, say German, please follow these steps.

1.  Fork the repository and create a new branch with the language name:

        git checkout -b german

2.  Create a new file `probstat-german.dict` based on `probstat-english.dict`:

        cp probstat-{english,german}.dict

3.  Make your changes. A dictionary consists of lines of key-value pairs, where
    the key is the immutable unique English identifier for a text snippet and
    the value is the corresponding translation. For example, the line

        \newtranslation{maintitle}{Probability and Statistics}

    translates the key `maintitle` to `Probability and Statistics`. The default
    dictionary is English. Because English is just yet another language, you
    will see a lot of lines of the form

        \newtranslation{discrete}{discrete}

    where key and value are the same. When creating a new dictionary, simply
    leave the key intact and change value. For example, to translate the above
    line to German, you would change it to:

        \newtranslation{discrete}{diskret}

    Finally, the first line of your new dictionary should convey the name of the
    new language, e.g.,

        \ProvidesDictionary{probstat}{german}

4.  Add the new file to the repository:

        git add probstat-german.dict
        git commit -a -m 'Add a German dictionary.'

5.  [Create a pull request][pull request] so that I can integrate your
    translation.

Tweaking Existing Translations
==============================

If you are unhappy with a certain translation or have suggestions for
improvements, you have two options.

1.  [Open a new issue][new issue] and contact the maintainer of the
    corresponding language to discuss the changes.

2.  [Create a pull request][pull request] and contact the maintainer(s). Once
    the maintainer(s) give green light, I pull in the changes.

[translator]: http://www.bakoma-tex.com/doc/latex/translator/
[new issue]: https://github.com/mavam/stat-cookbook/issues/new
[pull request]: https://github.com/mavam/stat-cookbook/pull/new/master
