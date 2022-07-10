#!/usr/bin/env python3

import os
import urllib.request
import urllib.parse
import urllib.error
import json
import PIL.Image
import PIL.ImageDraw

SCRYFALL_API = "https://api.scryfall.com/"
JSON_LOCATION = os.path.join(os.environ["HOME"], ".mtg-inspirational")
# If True, fetch a local image instead of hitting scryfall API
DEBUG = True

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
    { "image":  <image: PIL.Image>,
      "artist": <artist: str>,
      "text":     <flavour: str>
    }
    '''
    if DEBUG:
        return {
            "image": PIL.Image.open(f"{JSON_LOCATION}/mana-leak.jpg"),
            "artist": "Christopher Rush",
            "text": "The fatal flaw in every plan is the assumption that you know more than your enemy."
        }
    c = get_random_card(scryfall_args)
    return {
        "image": image_from_url(c["image_uris"]["art_crop"]),
        "artist": c.get("artist", ""),
        "text": c.get("flavor_text", "")
    }

def image_from_url(url):
    with urllib.request.urlopen(url) as f:
        return PIL.Image.open(f)

def crop_to_size(image, res_tuple):
    '''
    Scales the image up or down, and crops to fit into the new aspect ratio.
    '''
    new_width, new_height = res_tuple
    if new_width > image.width or new_height > image.height:
        print(f"Old image size is {image.size}, new is {res_tuple}. Not cropping.")
        return image
    else:
        return image.crop((0, new_width, 0, new_height))

def captioned_image(image_dict):
    # base = image_dict["image"].convert("RGBA")
    orig = image_dict["image"].convert("RGBA")
    base = crop_to_size(orig, (640, 400))
    txt = PIL.Image.new("RGBA", base.size, (255, 255, 255, 0))
    d = PIL.ImageDraw.Draw(txt)
    caption = image_dict["text"]
    d.text((10, 11), caption, fill=(0, 0, 0, 255))
    d.text((10, 10), caption, fill=(255, 255, 255, 255))
    return PIL.Image.alpha_composite(base, txt)

def basic_with_overlay(text):
    c = random_card_data("t:basic unique:art")
    c["text"] = text
    img = captioned_image(c)
    os.mkdir(JSON_LOCATION)
    with open(f"{JSON_LOCATION}/temp.png", "wb") as f:
        img.save(f)

def random_flavour_text():
    c = random_card_data("has:ft unique:art")
    out = captioned_image(c)
    with open(f"{JSON_LOCATION}/temp.png", "wb") as f:
        out.save(f)
    #out.show()

def main():
    random_flavour_text()

if __name__ == "__main__":
    main()
