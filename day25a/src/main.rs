#[derive(Debug)]
struct Snafu(String);

impl From<Snafu> for i128 {
    fn from(snafu: Snafu) -> Self {
        let chars = snafu.0.chars().rev();

        let mut acc = 0;
        for (i, c) in chars.enumerate() {
            let v = match c {
                '0' => 0,
                '1' => 1,
                '2' => 2,
                '-' => -1,
                '=' => -2,
                _ => panic!("Invalid input"),
            };

            let i = i as u32;
            acc += 5u128.pow(i) as i128 * v;
        }

        acc
    }
}

impl From<i128> for Snafu {
    fn from(mut val: i128) -> Self {
        let mut str = String::new();
        let mut carry = 0;

        while val != 0 || carry > 0 {
            let mut digit = val % 5 + carry;

            if digit > 2 {
                carry = 1;
                digit -= 5;
            } else {
                carry = 0;
            }

            str += match digit {
                -2 => "=",
                -1 => "-",
                0 => "0",
                1 => "1",
                2 => "2",
                _ => unreachable!(),
            };

            val /= 5;
        }

        Snafu(str.chars().rev().collect::<String>())
    }
}

fn main() {
    let sum: i128 = std::io::stdin()
        .lines()
        .map(|l| Snafu(l.unwrap()))
        .map(i128::from)
        .sum();

    println!("Sum: {sum:?}");

    let snafu_sum: Snafu = sum.into();

    println!("Sum: {snafu_sum:?}");

    let snafu_sum: i128 = snafu_sum.into();

    println!("Sum: {:?}", snafu_sum)
}
