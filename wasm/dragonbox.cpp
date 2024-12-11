#include <bit>
#include <cmath>
#include <cstdint>
#include <emscripten.h>
#include <vector>
#include "./dragonbox/dragonbox.hpp"

// output:
//   d[0 + i] = significand
//     0: use exponent bits as raw f32 bits
//   d[1 + i] = exponent
//   d[2 + i] =
//     0: positive
//     1: negative
EMSCRIPTEN_KEEPALIVE
extern "C" int* bulk_dragonbox(const std::int32_t* bitints, const int count) {
    static std::vector<int> result;
    
    const int bufferLengthNeeded = count * 3;
    
    result.resize(0);
    
    for(int i = 0; i < count; i++) {
        float f32 = std::bit_cast<float>(bitints[i]);
        
        if(f32 == 0.0 || f32 == -0.0 || std::isinf(f32) || std::isnan(f32)) {
            result.push_back(0);
            result.push_back(bitints[i]);
            result.push_back(0);
        } else {
            auto v = jkj::dragonbox::to_decimal(f32);
        
            result.push_back(std::bit_cast<std::int32_t>(v.significand));
            result.push_back(v.exponent);
            result.push_back(v.is_negative ? 1 : 0);
        }
    }
    
    return result.data();
}
