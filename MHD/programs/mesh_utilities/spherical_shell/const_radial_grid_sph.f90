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
      use m_spheric_parameter
!
      use const_sph_radial_grid
!
      implicit none
!
!
      integer(kind = kint) :: nele
      real(kind = kreal) :: shell, ratio, rmin, rmax
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
      read(*,*) sph1%sph_params%iflag_radial_grid
      if(sph1%sph_params%iflag_radial_grid .ne. 0) then
        sph1%sph_params%iflag_radial_grid = 2
      end if
!
      sph1%sph_params%radius_ICB = shell * ratio / (one - ratio)
      sph1%sph_params%radius_CMB = sph1%sph_params%radius_ICB + shell
!
      if(rmin.lt.0.0d0 .or. rmax.lt.0.0d0 .or. rmin.ge.rmax) then
        rmin = sph1%sph_params%radius_ICB
        rmax = sph1%sph_params%radius_CMB
      end if
!
      call count_set_radial_grid                                        &
     &   (nele, rmin, rmax, sph1%sph_params, sph1%sph_rtp)
!
      stop
      end program const_radial_grid_sph
