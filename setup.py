from setuptools import setup, find_packages

setup(
    name="mipyfive",
    version="0.1",
    description="Yet another 32-bit RISC-V soft-core",
    author="devbored",
    author_email="devbored.io@gmail.com",
    license="MPL-2.0",
    python_requires=">=3.7",
    install_requires=[
        "nmigen",
        # Uncomment when this PyPI package is updated
        #"riscv-assembler"
    ],
    packages=find_packages(),
    project_urls={
        #"Documentation": "Somewhere on my devbored.io site when I get around to it"
        "Source Code": "https://github.com/devbored/mipyfive"
    }
)
