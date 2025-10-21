mod icons;
mod style;

use icons::Icons;
use style::Style;

#[derive(Debug, Default)]
pub struct Ctx {
    pub style: Style,

    pub icons: Icons,
}
