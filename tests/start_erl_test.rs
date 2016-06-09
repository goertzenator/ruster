use std::process;

#[test]
fn main() {
	let result = process::Command::new("make")
			     //.arg("V=1")
			     .arg("tests")
				 .current_dir("tests/test_app")
				 .status();
	let success = match result {
		Ok(status) => status.success(),
		_ => false,
	};
	assert_eq!(true, success);
}
