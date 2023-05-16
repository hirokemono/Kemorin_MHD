!>@file   sph_dynamic_elsasser.f90
!!        program sph_dynamic_elsasser
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Find maximum degree and order of the spectrum
!
      program sph_dynamic_elsasser
!
      use m_precision
      use m_constants
!
      use m_sph_dynamic_elsasser
      use t_ctl_data_get_dyn_elsasser
      use t_ctl_param_sph_series_util
!
      implicit none
!
      type(get_dyn_elsasser_ctl), save :: elsasser_ctl1
      type(sph_dyn_elsasser_data), save :: els_dat1
!
!
      call read_ctl_file_get_dyn_elsasser(0, elsasser_ctl1)
      call set_sph_dyn_elsasser_params(elsasser_ctl1, els_dat1)
      call dealloc_ctl_get_dyn_elsasser(elsasser_ctl1)
!
      call sph_dynamic_elsasser_by_spectr(els_dat1)
      stop
!
      end program sph_dynamic_elsasser
!
