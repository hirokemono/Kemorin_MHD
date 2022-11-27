!>@file   t_ctl_param_dipole_fit.f90
!!        module t_ctl_param_dipole_fit
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Obtain lengh scale from spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine set_sph_dipole_fit_params(d_fit_ctl, fit_dat)
!!        type(dipole_fit_ratio_ctl), intent(in) :: d_fit_ctl
!!        type(dipole_fit_ratio_data), intent(inout) :: fit_dat
!!@endverbatim
!
      module t_ctl_param_dipole_fit
!
      use m_precision
      use m_constants
      use t_ctl_data_dipole_fit_ratio
      use t_read_sph_spectra
!
      implicit none
!
      type dipole_fit_ratio_data
        character(len = kchara)                                         &
     &            :: layer_l_spectr_file_name = 'sph_pwr_layer_l.dat'
        character(len = kchara)                                         &
     &            :: layer_m_spectr_file_name = 'sph_pwr_layer_m.dat'
        character(len = kchara)                                         &
     &            :: fit_ratio_file_name = 'fitted_ratio.dat'
!
        logical :: flag_old_spectr_data = .FALSE.
!
        real(kind = kreal) :: start_time
        real(kind = kreal) :: end_time
!
        logical :: flag_odd_only = .FALSE.
        integer(kind = kint) :: ltr_fit
        integer(kind = kint) :: ltr_dipolarity
!
        integer(kind = kint) :: ir_mpol =     0
        integer(kind = kint) :: num_fitting = 0
        integer(kind = kint) :: increment =   1
        real(kind = kreal), allocatable :: mpol_odd_ln_CMB(:)
        real(kind = kreal), allocatable :: dble_odd_degree(:)
      end type dipole_fit_ratio_data
!
      character(len = kchara), parameter, private                       &
     &                                   :: me_pol = 'M_ene_pol'
!
      integer(kind = kint), parameter, private :: ist_fdip = 1
      integer(kind = kint), parameter, private :: ist_fitb = 2
      integer(kind = kint), parameter, private :: ist_fit_coef =  5
      integer(kind = kint), parameter, private :: ist_sdev_coef = 7
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_sph_dipole_fit_params(d_fit_ctl, fit_dat)
!
      use skip_comment_f
!
      type(dipole_fit_ratio_ctl), intent(in) :: d_fit_ctl
      type(dipole_fit_ratio_data), intent(inout) :: fit_dat
!
!
      if(d_fit_ctl%layer_degree_spectr_file_name%iflag .gt. 0) then
        fit_dat%layer_l_spectr_file_name                                &
     &     = d_fit_ctl%layer_degree_spectr_file_name%charavalue
      end if
      if(d_fit_ctl%layer_order_spectr_file_name%iflag .gt. 0) then
        fit_dat%layer_m_spectr_file_name                                &
     &     = d_fit_ctl%layer_order_spectr_file_name%charavalue
      end if
      if(d_fit_ctl%fitted_ratio_file_name%iflag .gt. 0) then
        fit_dat%fit_ratio_file_name                                     &
     &     = d_fit_ctl%fitted_ratio_file_name%charavalue
      end if
!
!
      fit_dat%flag_old_spectr_data = .FALSE.
      if(d_fit_ctl%old_format_ctl%iflag .gt. 0) then
        fit_dat%flag_old_spectr_data                                    &
     &     = yes_flag(d_fit_ctl%old_format_ctl%charavalue)
      end if
!
!
      if(d_fit_ctl%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      fit_dat%start_time = d_fit_ctl%start_time_ctl%realvalue
!
      if(d_fit_ctl%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      fit_dat%end_time = d_fit_ctl%end_time_ctl%realvalue
!
!
      fit_dat%flag_odd_only = .FALSE.
      if(d_fit_ctl%odd_mode_only_ctl%iflag .gt. 0) then
        if(yes_flag(d_fit_ctl%odd_mode_only_ctl%charavalue))            &
     &     fit_dat%flag_odd_only = .TRUE.
      end if
!
      fit_dat%ltr_fit = -1
      if(d_fit_ctl%fit_truncation_ctl%iflag .gt. 0) then
        fit_dat%ltr_fit = d_fit_ctl%fit_truncation_ctl%intvalue
      end if
!
      fit_dat%ltr_dipolarity = -1
      if(d_fit_ctl%fdip_truncation_ctl%iflag .gt. 0) then
        fit_dat%ltr_dipolarity = d_fit_ctl%fdip_truncation_ctl%intvalue
      end if
!
      end subroutine set_sph_dipole_fit_params
!
!   --------------------------------------------------------------------
!
      subroutine init_dipole_fitting_data(sph_IN, fit_dat)
!
      use m_read_sph_spectra_f
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(dipole_fit_ratio_data), intent(inout) :: fit_dat
!
      integer(kind = kint) :: l, lcou
!
!
      fit_dat%ir_mpol = find_monitor_field_address(me_pol, sph_IN)
      if(fit_dat%ir_mpol .le. 0) then
        write(*,*) 'Magnetic energy data is missing.'
        stop
      end if
!
      if(fit_dat%ltr_dipolarity .gt. sph_IN%ltr_sph                     &
     &   .or. fit_dat%ltr_dipolarity .le. 1) then
        fit_dat%ltr_dipolarity = sph_IN%ltr_sph
      end if
      if(fit_dat%ltr_fit .gt. sph_IN%ltr_sph                            &
     &   .or. fit_dat%ltr_fit .le. 1) then
        fit_dat%ltr_fit = sph_IN%ltr_sph
      end if

      allocate(fit_dat%mpol_odd_ln_CMB(sph_IN%ltr_sph))
      allocate(fit_dat%dble_odd_degree(sph_IN%ltr_sph))
      fit_dat%mpol_odd_ln_CMB = 1.0d-30
!
      fit_dat%increment = 1
      if(fit_dat%flag_odd_only) fit_dat%increment = 2
      lcou = 0
      do l = 1, fit_dat%ltr_fit, fit_dat%increment
        lcou = lcou + 1
        fit_dat%dble_odd_degree(lcou) = dble(l)
      end do
      fit_dat%num_fitting = lcou
!
      end subroutine init_dipole_fitting_data
!
!   --------------------------------------------------------------------
!
      subroutine set_dipole_fitting_name(sph_IN, num_labels,            &
     &          num_time_labels, ene_sph_spec_name)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      integer(kind = kint), intent(in) :: num_labels, num_time_labels
!
      character(len = kchara), intent(inout)                            &
     &                        :: ene_sph_spec_name(num_labels)
!
      integer(kind = kint) :: num_tlabel
!
!
      num_tlabel = num_time_labels
      ene_sph_spec_name(1:num_tlabel)                                   &
     &         = sph_IN%ene_sph_spec_name(1:num_tlabel)
!
      ene_sph_spec_name(num_tlabel+ist_fdip) = 'dipolarity'
      ene_sph_spec_name(num_tlabel+ist_fitb  )                          &
     &         = 'dipole_fitted_ratio'
      ene_sph_spec_name(num_tlabel+ist_fitb+1)                          &
     &         = 'dipole_fitted_ratio_min'
      ene_sph_spec_name(num_tlabel+ist_fitb+2)                          &
     &         = 'dipole_fitted_ratio_max'
!
      ene_sph_spec_name(num_tlabel+ist_fit_coef) = 'Fit_const'
      ene_sph_spec_name(num_tlabel+ist_fit_coef+1)= 'Fit_base'
!
      ene_sph_spec_name(num_tlabel+ist_sdev_coef  ) = 'Sdev_const'
      ene_sph_spec_name(num_tlabel+ist_sdev_coef+1) = 'Sdev_base'
!
      end subroutine set_dipole_fitting_name
!
!   --------------------------------------------------------------------
!
      subroutine cal_dipole_fitting_ratio(sph_IN, ntot_sph_spec,        &
     &          kr_sph, r_sph, dipole_ratio, fit_dat)
!
      use approximate_to_polynomial
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      integer(kind = kint), intent(in) :: ntot_sph_spec
!
      integer(kind = kint), intent(inout) :: kr_sph
      real(kind = kreal), intent(inout) :: r_sph
      real(kind = kreal), intent(inout) :: dipole_ratio(ntot_sph_spec)
      type(dipole_fit_ratio_data), intent(inout) :: fit_dat
!
      real(kind = kreal) :: me_cmb_d, pwr_g10
      real(kind = kreal) :: fitted_l1_ref, fitted_l1_min, fitted_l1_max
      real(kind = kreal) :: COEF(0:1)
      real(kind = kreal) :: STDERR(0:1)
!
      integer(kind = kint) :: kr, l, lcou
!
!
      me_cmb_d = 0.0d0
      pwr_g10 = 0.0d0
      do kr = 1, sph_IN%nri_sph
        if(sph_IN%kr_sph(kr) .eq. sph_IN%kr_CMB) then
          kr_sph = sph_IN%kr_CMB
          r_sph =  sph_IN%r_sph(kr)
          pwr_g10 = sph_IN%spectr_IO(fit_dat%ir_mpol,1,kr)
!
          me_cmb_d = 0.0d0
          do l = 1, fit_dat%ltr_dipolarity
            me_cmb_d = me_cmb_d                                         &
     &                + sph_IN%spectr_IO(fit_dat%ir_mpol,l,kr)
          end do
!
          lcou = 0
          do l = 1, fit_dat%ltr_fit, fit_dat%increment
            lcou = lcou + 1
            fit_dat%mpol_odd_ln_CMB(lcou)                               &
     &             = log(sph_IN%spectr_IO(fit_dat%ir_mpol,l,kr))
          end do
          exit
        end if
      end do
!
      call approx_poly((fit_dat%num_fitting-1), ione,                   &
     &                 fit_dat%dble_odd_degree(2),                      &
     &                 fit_dat%mpol_odd_ln_CMB(2), COEF(0), STDERR(0))
!
      dipole_ratio(ist_fdip) = pwr_g10 / me_cmb_d
!
      fitted_l1_ref = exp(COEF(0) + COEF(1))
      fitted_l1_max = exp(COEF(0)+STDERR(0) + COEF(1)+STDERR(1))
      fitted_l1_min = exp(COEF(0)-STDERR(0) + COEF(1)-STDERR(1))
!
      dipole_ratio(ist_fitb  ) = pwr_g10 / fitted_l1_ref
      dipole_ratio(ist_fitb+1) = pwr_g10 / fitted_l1_max
      dipole_ratio(ist_fitb+2) = pwr_g10 / fitted_l1_min
      dipole_ratio(ist_fit_coef  ) =  exp(COEF(0))
      dipole_ratio(ist_fit_coef+1) =  exp(COEF(1))
      dipole_ratio(ist_sdev_coef  ) = exp(STDERR(0))
      dipole_ratio(ist_sdev_coef+1) = exp(STDERR(1))
!
     end subroutine cal_dipole_fitting_ratio
!
!   --------------------------------------------------------------------
!
      end module t_ctl_param_dipole_fit
