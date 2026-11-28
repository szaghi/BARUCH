call compare('emphasis', '*Hello*, world', '_Hello_, world')
call compare('heading and bullets', '# Title'//LF//LF//'- one'//LF//'- two', &
                                    'Title'//LF//'====='//LF//LF//'+ one'//LF//'+ two')
call compare('strong for emphasis', '- one'//LF//LF//'1. *two*', '- one'//LF//LF//'1. **two**')
call compare('a definition', '[BARUCH](https://github.com/szaghi/BARUCH)', &
                             '[BARUCH]'//LF//LF//'[baruch]: https://github.com/szaghi/BARUCH')
