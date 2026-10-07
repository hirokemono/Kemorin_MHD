#!/usr/bin/env python3
"""Compare a full-shell ASCII legacy VTK snapshot with the monitor.

Usage: check_snapshot.py SNAPSHOT.vtk[.gz] MONITOR.dat STEP NTH NPHI
Assumes the snapshot contains exactly the ICB-CMB radial grid. Integrates
unique Gaussian physical-grid nodes, excluding visualization pole copies.
This reproduces spherical quadrature, rather than VTK element integration.
"""
import gzip
import sys
from pathlib import Path
import numpy as np

snapshot, monitor, step, nth, nphi = sys.argv[1:]
step, nth, nphi = map(int, (step, nth, nphi))
opener = gzip.open if snapshot.endswith('.gz') else open
# Stream past connectivity/other fields instead of retaining a large VTK file.
with opener(snapshot, 'rt') as stream:
    point_line = next(line for line in stream if line.strip().startswith('POINTS '))
    n = int(point_line.split()[1])
    xyz = np.loadtxt(stream, max_rows=n)
    next(line for line in stream if line.strip().startswith('VECTORS magnetic_field '))
    b = np.loadtxt(stream, max_rows=n)
assert xyz.shape == b.shape == (n, 3)
r = np.linalg.norm(xyz, axis=1)
assert np.all(r > 0), 'This check expects a shell, not a full sphere'
radii, radial_index = np.unique(np.round(r, 12), return_inverse=True)
# Recover each radius at full exported precision.
radii = np.bincount(radial_index, weights=r) / np.bincount(radial_index)
wr = np.zeros(len(radii))
wr[:-1] += np.diff(radii) / 2
wr[1:] += np.diff(radii) / 2
wr *= radii**2
mu, wt = np.polynomial.legendre.leggauss(nth)
mu_node = xyz[:, 2] / r
right = np.clip(np.searchsorted(mu, mu_node), 0, nth - 1)
left = np.maximum(right - 1, 0)
latitude_index = np.where(abs(mu_node - mu[left]) < abs(mu_node - mu[right]), left, right)
phi = np.mod(np.arctan2(xyz[:, 1], xyz[:, 0]), 2 * np.pi)
longitude_index = np.rint(phi * nphi / (2 * np.pi)).astype(int) % nphi
physical = abs(mu_node - mu[latitude_index]) <= 1e-10
keys = ((radial_index[physical] * nth + latitude_index[physical]) * nphi
        + longitude_index[physical])
unique, first, inverse = np.unique(keys, return_index=True, return_inverse=True)
assert len(unique) == len(radii) * nth * nphi, 'Incomplete physical-grid snapshot'
bz_all = b[physical, 2]
bz = bz_all[first]
np.testing.assert_allclose(bz[inverse], bz_all, rtol=1e-10, atol=1e-12)
k = unique // (nth * nphi)
l = (unique // nphi) % nth
weights = wr[k] * wt[l] * (2 * np.pi / nphi)
values = np.array([np.dot(weights, bz), np.dot(weights, np.maximum(bz, 0)),
                   np.dot(weights, np.maximum(-bz, 0))])
rows = np.atleast_2d(np.loadtxt(monitor))
row = rows[rows[:, 0] == step]
assert len(row) == 1, 'Expected one monitor record for this step'
np.testing.assert_allclose(values, row[0, 2:5], rtol=1e-11, atol=1e-11)
print('PASS: snapshot quadrature agrees; maximum difference', max(abs(values - row[0, 2:5])))
