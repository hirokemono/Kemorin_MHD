!>@file   t_ctl_data_get_dyn_elsasser.f90
!!        module t_ctl_data_get_dyn_elsasser
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_ctl_file_get_dyn_elsasser(my_rank, elsasser_ctl)
!!      subroutine read_ctl_get_dyn_elsasser                            &
!!        type(get_dyn_elsasser_ctl), intent(inout) :: elsasser_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dealloc_ctl_get_dyn_elsasser(elsasser_ctl)
!!        type(get_dyn_elsasser_ctl), intent(inout) :: elsasser_ctl
!! -----------------------------------------------------------------
!!
!!      control block for dynamic Elsasser number
!!
!!  begin sph_elsasser_numbers
!!    start_time_ctl     2.0
!!    end_time_ctl       2.5
!!  
!!    old_format_flag     'Off'
!!    vol_degree_spectr_file_name     'sph_pwr_volume_l.dat.gz'
!!    vol_order_spectr_file_name      'sph_pwr_volume_m.dat.gz'
!!    elsasser_numbers_file_name      'Elsasser.dat'
!!
!!    begin dimensionless_ctl
!!      array dimless_ctl
!!        dimless_ctl  Prandtl_number                   1.0e-0
!!        dimless_ctl  modified_Rayleigh_number         1.0E+2
!!        dimless_ctl  Ekman_number                     1.0e-3
!!        dimless_ctl  magnetic_Prandtl_number          5.0e+0
!!        dimless_ctl  Composite_Rayleigh_number        1.0E+2
!!        dimless_ctl  Composite_Prandtl_number         1.0E+2
!!      end array dimless_ctl
!!    end  dimensionless_ctl
!!
!!    array mag_to_kin_energy_ratio
!!      mag_to_kin_energy_ratio   Ekman              -1.0
!!      mag_to_kin_energy_ratio   magnetic_Prandtl   -1.0
!!    end array mag_to_kin_energy_ratio
!!    array magnetic_Reynolds_ratio
!!      magnetic_Reynolds_ratio   magnetic_Prandtl   1.0
!!    end array magnetic_Reynolds_ratio
!!    array Coefs_for_Elsasser
!!      Coefs_for_Elsasser   Ekman               1.0
!!      Coefs_for_Elsasser   magnetic_Prandtl    1.0
!!    end array Coefs_for_Elsasser
!!
!!  end sph_elsasser_numbers
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_get_dyn_elsasser
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_charareal
      use t_ctl_data_dimless_numbers
!
      implicit  none
!
!>        Control file name
      character(len = kchara), parameter, private                       &
     &           :: fname_ctl_sph_elsasser = 'control_sph_elsasser'
!>        Control file ID
      integer(kind = kint), parameter, private :: id_control = 11
!
!
      type get_dyn_elsasser_ctl
!>        Structure for start time
        type(read_real_item) :: start_time_ctl
!>        Structure for end time
        type(read_real_item) :: end_time_ctl
!
!>        Character flag for old format
        type(read_character_item) :: old_format_ctl
!
!>        Structure for Elsasser number
        type(read_character_item) :: Elsasser_file_name_ctl
!
!>        Structure for Elsasser number
        type(read_character_item) :: vol_degree_spectr_file_name
!>        Structure for Elsasser number
        type(read_character_item) :: vol_order_spectr_file_name
!
!>        Structure for list of dimensionless numbers
        type(dimless_control) :: dless_ctl
!
!>        Ratio of Magnetic energy to kinetic energy
        type(ctl_array_cr) :: ME_to_KE_ctl
!>        Coefficients to construct magnetic Reynolds number
        type(ctl_array_cr) :: mag_Re_coefs
!>        Coefficients to construct Elsasser number
        type(ctl_array_cr) :: Elsasser_coefs
!
        integer (kind = kint) :: i_gen_dyn_elsasser = 0
      end type get_dyn_elsasser_ctl
!
!
!    label for entry
!
      character(len=kchara), parameter, private                         &
     &             :: hd_gen_elsasser = 'sph_elsasser_numbers'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_start_time_ctl = 'start_time_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_end_time_ctl = 'end_time_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_old_format =   'old_format_flag'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_vol_spec_l_file = 'vol_degree_spectr_file_name'
      character(len=kchara), parameter, private                         &
     &           :: hd_vol_spec_m_file = 'vol_order_spectr_file_name'
      character(len=kchara), parameter, private                         &
     &           :: hd_Elsasser_file = 'elsasser_numbers_file_name'
      character(len=kchara), parameter, private                         &
     &           :: hd_dimless_list = 'dimensionless_ctl'
!
      character(len=kchara), parameter, private                         &
     &        :: hd_mag_to_kin_ratio = 'mag_to_kin_energy_ratio'
      character(len=kchara), parameter, private                         &
     &        :: hd_mag_reynolds_coefs = 'magnetic_Reynolds_ratio'
      character(len=kchara), parameter, private                         &
     &        :: hd_elsasser_coefs =   'Coefs_for_Elsasser'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_ctl_file_get_dyn_elsasser(my_rank, elsasser_ctl)
!
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(get_dyn_elsasser_ctl), intent(inout) :: elsasser_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        open(id_control, file = fname_ctl_sph_elsasser, status='old')
        do
          call load_one_line_from_control                               &
     &       (id_control, hd_gen_elsasser, c_buf1)
          if(c_buf1%iend .gt. 0) exit
!
          call read_ctl_get_dyn_elsasser                                &
     &       (id_control, hd_gen_elsasser, elsasser_ctl, c_buf1)
          if(elsasser_ctl%i_gen_dyn_elsasser .gt. 0) exit
        end do
        close(id_control)
      end if
!
      end subroutine read_ctl_file_get_dyn_elsasser 
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_get_dyn_elsasser                              &
     &         (id_control, hd_block, elsasser_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(get_dyn_elsasser_ctl), intent(inout) :: elsasser_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(elsasser_ctl%i_gen_dyn_elsasser  .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_start_time_ctl, elsasser_ctl%start_time_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_end_time_ctl, elsasser_ctl%end_time_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_old_format, elsasser_ctl%old_format_ctl)
!
        call read_chara_ctl_type(c_buf, hd_Elsasser_file,               &
     &      elsasser_ctl%Elsasser_file_name_ctl)
!
        call read_chara_ctl_type(c_buf, hd_vol_spec_l_file,             &
     &      elsasser_ctl%vol_degree_spectr_file_name)
        call read_chara_ctl_type(c_buf, hd_vol_spec_m_file,             &
     &      elsasser_ctl%vol_order_spectr_file_name)
!
        call read_dimless_ctl                                           &
     &     (id_control, hd_dimless_list, elsasser_ctl%dless_ctl, c_buf)
!
        call read_control_array_c_r(id_control, hd_mag_to_kin_ratio,    &
     &      elsasser_ctl%ME_to_KE_ctl, c_buf)
        call read_control_array_c_r(id_control, hd_mag_reynolds_coefs,  &
     &      elsasser_ctl%mag_Re_coefs, c_buf)
        call read_control_array_c_r(id_control, hd_elsasser_coefs,      &
     &      elsasser_ctl%Elsasser_coefs, c_buf)
      end do
      elsasser_ctl%i_gen_dyn_elsasser = 1
!
      end subroutine read_ctl_get_dyn_elsasser
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ctl_get_dyn_elsasser(elsasser_ctl)
!
      type(get_dyn_elsasser_ctl), intent(inout) :: elsasser_ctl
!
!
      elsasser_ctl%start_time_ctl%iflag =   0
      elsasser_ctl%end_time_ctl%iflag =     0
      elsasser_ctl%old_format_ctl%iflag =   0
!
      call dealloc_dimless_ctl(elsasser_ctl%dless_ctl)
!
      elsasser_ctl%Elsasser_file_name_ctl%iflag = 0
!
      elsasser_ctl%vol_degree_spectr_file_name%iflag = 0
      elsasser_ctl%vol_order_spectr_file_name%iflag =  0
!
      call dealloc_control_array_c_r(elsasser_ctl%ME_to_KE_ctl)
      call dealloc_control_array_c_r(elsasser_ctl%mag_Re_coefs)
      call dealloc_control_array_c_r(elsasser_ctl%Elsasser_coefs)
!
      elsasser_ctl%i_gen_dyn_elsasser = 0
!
      end subroutine dealloc_ctl_get_dyn_elsasser
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_get_dyn_elsasser
