<pre>
%{
fn wman_page_gen {
    man -l $1 | escape_html
}
wman_page_gen /var/www/sigkill.dk/pub/code/gsmenu/gsmenu.1
%}
</pre>
