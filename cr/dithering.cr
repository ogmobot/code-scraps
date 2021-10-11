require "stumpy_png"
include StumpyPNG

def brightness (pixel)
    return (pixel.r.to_u32 + pixel.g + pixel.b) // 3
end

def round_u8 (val, num_values)
    # when num_values is 2, rounds val to 0 or 255;
    # when num_values is 3, rounds val to 0, 128 or 255;
    # when num_values is 5, rounds val to 0, 64, 128, 144 or 255;
    # etc.
    (1..num_values).each do |numerator|
        if val < ((256 // num_values) * numerator)
            return ((256.to_u16 // (num_values - 1)) * (numerator - 1)).clamp(0..255).to_u8
        end
    end
    return 255.to_u8
end

def dither_bw (input_canvas, output_filename, num_subcolours)
    output_canvas = Canvas.new(
        input_canvas.width,
        input_canvas.height)

    brightnesses = Array(UInt8).new()
    (0...(input_canvas.height)).each do |y|
        (0...(input_canvas.width)).each do |x|
            brightnesses << (brightness(input_canvas[x, y]) // 256).to_u8
        end
    end

    (0...(input_canvas.height)).each do |y|
        (0...(input_canvas.width)).each do |x|
            index = (input_canvas.width * y) + x
            output_brightness = round_u8(brightnesses[index], num_subcolours)
            diff = brightnesses[index].to_i32 - output_brightness.to_i32
            [
                {(input_canvas.width *  y     ) + x + 1, (diff * 7 // 16)},
                {(input_canvas.width * (y + 1)) + x - 1, (diff * 3 // 16)},
                {(input_canvas.width * (y + 1)) + x    , (diff * 5 // 16)},
                {(input_canvas.width * (y + 1)) + x + 1, (diff * 1 // 16)},
            ].each do |index, val|
                if index < brightnesses.size
                    new_brightness = (val + brightnesses[index]).clamp(0..255)
                    brightnesses[index] = new_brightness.to_u8
                end
            end

            output_canvas[x, y] = RGBA.from_rgb_n(
                output_brightness,
                output_brightness,
                output_brightness,
                8)
        end
    end

    StumpyPNG.write(
        output_canvas,
        output_filename,
        bit_depth: 8,
        colour_type: :grayscale)
end

def dither_colour (input_canvas, output_filename, num_subcolours)
    output_canvas = Canvas.new(
        input_canvas.width,
        input_canvas.height)

    bright_r = Array(UInt8).new()
    bright_g = Array(UInt8).new()
    bright_b = Array(UInt8).new()
    (0...(input_canvas.height)).each do |y|
        (0...(input_canvas.width)).each do |x|
            bright_r << ((input_canvas[x, y].r) // 256).to_u8
            bright_g << ((input_canvas[x, y].g) // 256).to_u8
            bright_b << ((input_canvas[x, y].b) // 256).to_u8
        end
    end

    (0...(input_canvas.height)).each do |y|
        (0...(input_canvas.width)).each do |x|
            index = (input_canvas.width * y) + x
            red   = round_u8(bright_r[index], num_subcolours)
            green = round_u8(bright_g[index], num_subcolours)
            blue  = round_u8(bright_b[index], num_subcolours)
            diff_r = bright_r[index].to_i32 - red.to_i32
            diff_g = bright_g[index].to_i32 - green.to_i32
            diff_b = bright_b[index].to_i32 - blue.to_i32
            [
                {(input_canvas.width *  y     ) + x + 1,
                    (diff_r * 7 // 16),
                    (diff_g * 7 // 16),
                    (diff_b * 7 // 16)},
                {(input_canvas.width * (y + 1)) + x - 1,
                    (diff_r * 3 // 16),
                    (diff_g * 3 // 16),
                    (diff_b * 3 // 16)},
                {(input_canvas.width * (y + 1)) + x    ,
                    (diff_r * 5 // 16),
                    (diff_g * 5 // 16),
                    (diff_b * 5 // 16)},
                {(input_canvas.width * (y + 1)) + x + 1,
                    (diff_r * 1 // 16),
                    (diff_g * 1 // 16),
                    (diff_b * 1 // 16)},
            ].each do |index, r, g, b|
                if index < bright_r.size
                    new_r = (r + bright_r[index]).clamp(0..255)
                    new_g = (g + bright_g[index]).clamp(0..255)
                    new_b = (b + bright_b[index]).clamp(0..255)
                    bright_r[index] = new_r.to_u8
                    bright_g[index] = new_g.to_u8
                    bright_b[index] = new_b.to_u8
                end
            end

            output_canvas[x, y] = RGBA.from_rgb_n(red, green, blue, 8)
        end
    end

    StumpyPNG.write(
        output_canvas,
        output_filename,
        bit_depth: 8,
        colour_type: :rgb)
end

puts "Enter filename."
filename = gets
input_canvas = if filename
    StumpyPNG.read(filename.strip)
else
    StumpyPNG.read("/dev/null")
end

dither_bw(input_canvas, "output.png", 4)
dither_colour(input_canvas, "output_colour.png", 4)
puts "Done."
