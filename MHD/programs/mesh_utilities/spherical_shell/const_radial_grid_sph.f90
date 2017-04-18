!const_radial_grid_sph.f90
!      program const_radial_grid_sph
!
      program const_radial_grid_sph
!
!      Written by Kemorin on Sep., 2009
!
      use m_precision
!
      use m_constants
      use t_spheric_global_ranks
      use t_spheric_parameter
      use t_spheric_rtp_data
!
      use const_sph_radial_grid
!
      implicit none
!
      integer(kind = kint) :: nele
      real(kind = kreal) :: shell, ratio, rmin, rmax
!
!>  Structure of grid and spectr data for spherical spectr method
      type(sph_shell_parameters) :: sph_params_rgrid
!>        structure of index table for @f$ f(r,\theta,\phi) @f$
      type(sph_rtp_grid) :: sph_rtp_rgrid
!
      type(spheric_global_radius), save :: s3d_radius
!
!
      write(*,*) 'input outer core shell width'
      read(*,*) shell
      write(*,*) 'input inner core / whole shell (< 0.5)'
      read(*,*) ratio
!
      write(*,*) 'input minimum and maximum radius of domain'
      read(*,*) rmin, rmax
!
      write(*,*) 'input number of layer for outer core'
      read(*,*) nele
!
      write(*,*) 'input grid type (0:equi-distance, 2:Chebyshev)'
      read(*,*) sph_params_rgrid%iflag_radial_grid
      if(sph_params_rgrid%iflag_radial_grid .ne. 0) then
        sph_params_rgrid%iflag_radial_grid = 2
      end if
!
      sph_params_rgrid%radius_ICB = shell * ratio / (one - ratio)
      sph_params_rgrid%radius_CMB = sph_params_rgrid%radius_ICB + shell
!
      if(rmin.lt.0.0d0 .or. rmax.lt.0.0d0 .or. rmin.ge.rmax) then
        rmin = sph_params_rgrid%radius_ICB
        rmax = sph_params_rgrid%radius_CMB
      end if
!
      call count_set_radial_grid                                        &
     &  (nele, rmin, rmax, sph_params_rgrid, sph_rtp_rgrid, s3d_radius)
!
      stop
      end program const_radial_grid_sph
