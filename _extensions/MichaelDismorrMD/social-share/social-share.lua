local function ensureHtmlDeps()
    quarto.doc.addHtmlDependency({
        name = 'social-share',
        version = '0.1.0',
        stylesheets = {
            'social-share.css',
            '_extensions/quarto-ext/fontawesome/assets/css/all.css'
        }
    })
end

function Meta(m)
    ensureHtmlDeps()

    -- Check if m.share exists, if not, return early
    if not m.share then
        return
    end

    local share_start = '<div class= "page-columns page-rows-contents page-layout-article"><div class="social-share">'
    if m.share.divclass then
        local divclass = pandoc.utils.stringify(m.share.divclass)
        share_start = '<div class= "' .. divclass .. '"><div class="social-share">'
    end
    local share_end = '</div></div>'
    local share_text = share_start

    -- Default to empty string for share_url, it will be set by JavaScript if not provided
    local share_url = ""
    if m.share.permalink then
        share_url = pandoc.utils.stringify(m.share.permalink)
    end

    local post_title = pandoc.utils.stringify(m.title)
    if m.share.description then
        post_title = pandoc.utils.stringify(m.share.description)
    end

    local function createShareLink(platform, base_url, params)
        return '<a href="' .. base_url .. '?' .. params .. '" target="_blank" class="' .. platform .. '"><i class="fab fa-' .. platform .. ' fa-fw fa-lg"></i></a>'
    end

    if m.share.twitter then
        share_text = share_text .. createShareLink('twitter', 'https://twitter.com/share', 'url=' .. share_url .. '&text=' .. post_title)
    end
    if m.share.linkedin then
        share_text = share_text .. createShareLink('linkedin', 'https://www.linkedin.com/shareArticle', 'url=' .. share_url .. '&title=' .. post_title)
    end
    if m.share.email then
        share_text = share_text .. '<a href="mailto:?subject=' .. post_title .. '&body=Check out this link: ' .. share_url .. '" target="_blank" class="email"><i class="fa-solid fa-envelope fa-fw fa-lg"></i></a>'
    end
    if m.share.facebook then
        share_text = share_text .. createShareLink('facebook', 'https://www.facebook.com/sharer.php', 'u=' .. share_url)
    end
    if m.share.reddit then
        share_text = share_text .. createShareLink('reddit', 'https://reddit.com/submit', 'url=' .. share_url .. '&title=' .. post_title)
    end
    if m.share.stumble then
        share_text = share_text .. createShareLink('stumbleupon', 'https://www.stumbleupon.com/submit', 'url=' .. share_url .. '&title=' .. post_title)
    end
    if m.share.tumblr then
        share_text = share_text .. createShareLink('tumblr', 'https://www.tumblr.com/share/link', 'url=' .. share_url .. '&name=' .. post_title)
    end
    if m.share.mastodon then
        share_text = share_text .. '<a href="javascript:void(0);" onclick="var mastodon_instance=prompt(\'Mastodon Instance / Server Name?\'); if(typeof mastodon_instance===\'string\' && mastodon_instance.length){this.href=\'https://\'+mastodon_instance+\'/share?text=' .. post_title .. ' ' .. share_url .. '\'}else{return false;}" target="_blank" class="mastodon"><i class="fa-brands fa-mastodon fa-fw fa-lg"></i></a>'
    end
    share_text = share_text .. share_end

    -- Add JavaScript to set the current page URL if not provided
    local js_script = [[
    <script>
    document.addEventListener("DOMContentLoaded", function() {
        if ("]] .. share_url .. [[" === "") {
            var currentUrl = window.location.href;
            var links = document.querySelectorAll('.social-share a');
            links.forEach(function(link) {
                var href = link.getAttribute('href');
                if (link.classList.contains('email')) {
                    link.setAttribute('href', 'mailto:?subject=]] .. post_title .. [[&body=Check out this link: ' + encodeURIComponent(currentUrl));
                } else {
                    link.setAttribute('href', href.replace('url=', 'url=' + encodeURIComponent(currentUrl)));
                }
            });
        }
    });
    </script>
    ]]

    share_text = share_text .. js_script

    if m.share.location then
        quarto.doc.includeText(pandoc.utils.stringify(m.share.location), share_text)
    else
        quarto.doc.includeText("after-body", share_text)
    end
end
