def parse_css(css):
    css_parts = css.split(";")
    ret = {}
    for part in css_parts:
        spart = part.split(":")
        if len(spart) < 2: continue
        key = spart[0].strip()
        value = spart[1].strip()
        ret[key] = value
    return ret

print (parse_css("background-color:red; top: 22px;"))
