#!/usr/bin/env python3

import os
import urllib.request
import urllib.parse
import urllib.error
import json
import PIL.Image
import PIL.ImageDraw
import PIL.ImageFont
import random
import subprocess
import hitherdither

SCRYFALL_API = "https://api.scryfall.com/"
JSON_LOCATION = os.path.join(os.environ["HOME"], ".mtg-inspirational")
FONT_LOCATION = "/usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf"
# If True, fetch a local image instead of hitting scryfall API
DEBUG = False
SOLID_BLACK = (0, 0, 0, 255)
SOLID_WHITE = (255, 255, 255, 255)
SHADOW_OFFSET = 2

def get_random_card(scryfall_args):
    params = urllib.parse.urlencode({"q": scryfall_args})
    url = f"{SCRYFALL_API}cards/random?{params}"
    print(url)
    with urllib.request.urlopen(url) as f:
        card = json.load(f)
    return card

def random_card_data(scryfall_args):
    '''
    Returns a dict:
    { "name":   <name: str>,
      "image":  <image: PIL.Image>,
      "artist": <artist: str>,
      "text":   <flavour: str>
    }
    '''
    debug_card = {
            "name": "Mana Leak (Eighth Edition)",
            "image": PIL.Image.open(f"{JSON_LOCATION}/mana-leak.jpg"),
            "artist": "Christopher Rush",
            "text": "The fatal flaw in every plan is the assumption that you know more than your enemy."
        }
    if DEBUG: return debug_card
    c = get_random_card(scryfall_args)
    if "card_faces" in c:
        c = random.choice(c["card_faces"])
    try:
        return {
            "name": c.get("name", "") + " (" + c.get("set", "").upper() + ")",
            "image": image_from_url(c["image_uris"]["art_crop"]),
            "artist": c.get("artist", ""),
            "text": c.get("flavor_text", "")
        }
    except KeyError as e:
        print(e)
        print(c)
        return debug_card

def image_from_url(url):
    with urllib.request.urlopen(url) as f:
        return PIL.Image.open(f)

def crop_to_size(image, res_tuple):
    '''
    Scales the image up or down, and crops to fit into the new aspect ratio.
    '''
    new_width, new_height = res_tuple
    old_width, old_height = image.size

    if new_width * old_height < old_width * new_height: # old is too wide; scale height
        scale_factor = new_height / old_height
    else: # old is too tall; scale width
        scale_factor = new_width / old_width
    tmp = image.resize((int(old_width * scale_factor) + 1, int(old_height * scale_factor) + 1))

    assert tmp.width >= new_width
    assert tmp.height >= new_height

    dw = tmp.width - new_width
    dh = tmp.height - new_height

    return tmp.crop((
        dw // 2, # left
        dh // 2, # top
        (dw // 2) + new_width, # right
        (dh // 2) + new_height) # bottom
    )

def wrap_text(text, width):
    lines = []
    paras = text.split("\n")
    for p in paras:
        line = []
        words = p.split(" ")
        for word in words:
            if len(f"{' '.join(line)} {word}") > width:
                lines.append(" ".join(line))
                line = []
            line.append(word)
        if line:
            lines.append(" ".join(line))
    return lines

def captioned_image(image_dict):
    print(f"Captioning card: \"{image_dict.get('name', '???')}\"")
    # base = image_dict["image"].convert("RGBA")
    orig = image_dict["image"].convert("RGBA")
    base = crop_to_size(orig, (640, 400))
    txt = PIL.Image.new("RGBA", base.size, (255, 255, 255, 0))
    caption_font = PIL.ImageFont.truetype(FONT_LOCATION, 18)
    artist_font = PIL.ImageFont.truetype(FONT_LOCATION, 14)
    d = PIL.ImageDraw.Draw(txt)
    caption = image_dict["text"]
    caption = "\n".join(wrap_text(caption, 56)) # trial and error - 18pt LiberationMono Bold, 640px
    artist_text = f"\"{image_dict['name']}\", illus. {image_dict['artist']}"
    d.multiline_text((txt.width // 2 + SHADOW_OFFSET, txt.height // 3 + SHADOW_OFFSET), caption, fill=SOLID_BLACK, anchor="mm", font=caption_font)
    d.multiline_text((txt.width // 2, txt.height // 3), caption, fill=SOLID_WHITE, anchor="mm", font=caption_font)
    d.text((txt.width - 10 + SHADOW_OFFSET, txt.height - 10 + SHADOW_OFFSET), artist_text, fill=SOLID_BLACK, anchor="rs", font=artist_font)
    d.text((txt.width - 10, txt.height - 10), artist_text, fill=SOLID_WHITE, anchor="rs", font=artist_font)
    return PIL.Image.alpha_composite(base, txt)

def basic_with_overlay(text):
    c = random_card_data("t:basic unique:art")
    c["text"] = text
    img = captioned_image(c)
    #with open(f"{JSON_LOCATION}/temp.png", "wb") as f:
        #img.save(f)
    return img

def random_flavour_text():
    c = random_card_data("has:ft unique:art")
    img = captioned_image(c)
    return img

def random_fortune():
    '''
    Use the `fortune -s` command and place the output on a basic land.
    '''
    fortune = subprocess.Popen(["fortune", "-s"], stdout=subprocess.PIPE)
    (output, err) = fortune.communicate()
    fortune.wait()
    text = output.decode()
    print("Fortune:")
    print(text)
    for a, b in [("\n\t", "\a"), ("\t", "  "), ("\n", " "), ("\a", "\n  ")]:
        text = text.replace(a, b)
    basic_with_overlay(text)

def main():
    #img = random_fortune()
    img = random_flavour_text()
    #with open(f"{JSON_LOCATION}/temp.png", "wb") as f:
        #img.save(f)
    img.show()

if __name__ == "__main__":
    main()
