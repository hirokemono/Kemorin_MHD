!m_ctl_data_mhd_evo_scheme.f90
!      module m_ctl_data_mhd_evo_scheme
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine read_restart_ctl
!      subroutine read_time_loop_ctl
!
!!!!!  control for initial and restart data  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      0: No initial values
!      1: Read restart data as initial values
!
!     -1: Initial values for dynamo benchmark Case 0
!     -2: Initial values for dynamo benchmark Case 1
!
!    -11: rotate around x-axis
!    -12: rotate around y-axis
!    -13: rotate around z-axis
!
!     20: Initial values for kinematic dynamo
!
!  <-100: Initial value for convection in rotating shell
!          int(num/100)... wave number in zonal direction
!  >1000: Initial value for MHD dynamo in rotating shell
!          int(num/100)... wave number of temperature in zonal direction
!          int(num/1000)... index j for spherical harmonics 
!                           by degree l and order m
!                                j = l*(l+1) + m
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    begin restart_file_ctl
!       rst_ctl                -2
!    end restart_file_ctl
!
!!!!!!   method for time evolution  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   iflag_supg_ctl:      0...no SUPG 1...SUPG
!   num_multi_pass_ctl:  iteration counts for multi pass
!   maxiter_ctl:         maximum iteration number for correction
!   eps_4_velo_ctl:      ||div v||_{n} / ||div v||_{n-1}
!   eps_4_magne_ctl:     ||div B||_{n} / ||div B||_{n-1}
!   scheme_ctl:          Scheme for time evolution
!                 explicit_Euler...explicit_Euler
!                 2nd_Adams_Bashforth...2nd_Adams_Bashforth
!                 Crank_Nicolson...Crank_Nicolson with 2nd_Adams_Bashforth
!                 Crank_Nicolson_consist...Crank_Nicolson
!                                         with consistent mass matrix
!   eps_crank_ctl:        
!   method_4_velo_ctl:    method for Crank Nicolson Scheme
!   precond_4_crank_ctl:  preconditioning method for Crank Nicolson Scheme
!
!   spherical transfer mode:  'radius_in' 'radius_out' 'long_loop'
!   FFT_library_ctl:  Selection of FFT librarry  ('ISPACK' or 'FFTPACK')
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    begin time_loop_ctl
!      iflag_supg_ctl           0
!      num_multi_pass_ctl       1
!      maxiter_ctl              1
!      eps_4_velo_ctl           5.0e-1
!      eps_4_magne_ctl          5.0e-1
!      scheme_ctl              Crank_Nicolson
!      diffuse_correct_ctl     On
!      coef_imp_v_ctl          5.0e-1
!      coef_imp_t_ctl          5.0e-1
!      coef_imp_b_ctl          5.0e-1
!      coef_imp_c_ctl          5.0e-1
!      eps_crank_ctl           1.0e-6
!      method_4_velo_ctl      CG 
!      precond_4_crank_ctl     SSOR   
!      modify_coriolis_4_crank_ctl  0
!      sph_transform_mode_ctl   'radius_in'
!      FFT_library_ctl          'ISPACK'
!    end time_loop_ctl
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_ctl_data_mhd_evo_scheme
!
      use m_precision
!
      implicit  none
!
!
      character(len=kchara) :: restart_flag_ctl
!   control flage for restart data
!
      real(kind=kreal) :: coef_imp_v_ctl
      real(kind=kreal) :: coef_imp_t_ctl
      real(kind=kreal) :: coef_imp_b_ctl
      real(kind=kreal) :: coef_imp_c_ctl
!
!
      integer(kind=kint) :: iflag_supg_ctl
      integer(kind=kint) :: num_multi_pass_ctl
      integer(kind=kint) :: maxiter_ctl
! 
      real(kind=kreal)   :: eps_4_velo_ctl
      real(kind=kreal)   :: eps_4_magne_ctl
      real(kind=kreal)   :: eps_crank_ctl
! 
      character (len=kchara)   :: scheme_ctl
      character (len=kchara)   :: diffuse_correct_ctl
! 
      character (len=kchara)   :: method_4_velo_ctl
      character (len=kchara)   :: precond_4_crank_ctl
! 
      character(len = kchara) :: sph_transform_mode_ctl
      character(len = kchara) :: FFT_library_ctl
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &      :: hd_restart_file =   'restart_file_ctl'
      integer (kind=kint) :: i_restart_file =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_loop =      'time_loop_ctl'
      integer (kind=kint) :: i_time_loop =      0
!
!    4th level for restart
!
      character(len=kchara), parameter :: hd_rst_flag = 'rst_ctl'
      integer (kind=kint) :: i_rst_flag = 0
!
!    4th level for time_loop_ctl
!
      character(len=kchara), parameter                                  &
     &      :: hd_iflag_supg =     'iflag_supg_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_num_multi_pass = 'num_multi_pass_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_maxiter =        'maxiter_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_eps_4_velo =     'eps_4_velo_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_eps_4_magne =    'eps_4_magne_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_scheme =         'scheme_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_diff_correct =   'diffuse_correct_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_imp_v =     'coef_imp_v_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_imp_t =     'coef_imp_t_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_imp_b =     'coef_imp_b_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_imp_c =     'coef_imp_c_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_eps_crank =      'eps_crank_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_method_4_velo =  'method_4_velo_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_precond_4_crank = 'precond_4_crank_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_sph_transform_mode =  'sph_transform_mode_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_FFT_package =  'FFT_library_ctl'
!
      integer (kind=kint) :: i_iflag_supg =         0
      integer (kind=kint) :: i_num_multi_pass =     0
      integer (kind=kint) :: i_maxiter =            0
      integer (kind=kint) :: i_eps_4_velo =         0
      integer (kind=kint) :: i_eps_4_magne =        0
      integer (kind=kint) :: i_scheme =             0
      integer (kind=kint) :: i_diff_correct =       0
      integer (kind=kint) :: i_coef_imp_v =         0
      integer (kind=kint) :: i_coef_imp_t =         0
      integer (kind=kint) :: i_coef_imp_b =         0
      integer (kind=kint) :: i_coef_imp_c =         0
      integer (kind=kint) :: i_eps_crank =          0
      integer (kind=kint) :: i_method_4_velo =      0
      integer (kind=kint) :: i_precond_4_crank =    0
      integer (kind=kint) :: i_sph_transform_mode = 0
      integer (kind=kint) :: i_FFT_package =        0
!
      private :: hd_restart_file, hd_rst_flag, i_restart_file
      private :: hd_time_loop, i_time_loop
      private :: hd_iflag_supg, hd_num_multi_pass, hd_maxiter
      private :: hd_eps_4_velo, hd_eps_4_magne, hd_scheme
      private :: hd_diff_correct, hd_coef_imp_v, hd_coef_imp_t
      private :: hd_coef_imp_b, hd_coef_imp_c, hd_eps_crank
      private :: hd_method_4_velo, hd_precond_4_crank
      private :: hd_sph_transform_mode
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_restart_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_restart_file) .eq. 0) return
      if (i_restart_file .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_restart_file, i_restart_file)
        if(i_restart_file .gt. 0) exit
!
        call read_character_ctl_item(hd_rst_flag,                       &
     &          i_rst_flag, restart_flag_ctl)
      end do
!
      end subroutine read_restart_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_time_loop_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_time_loop) .eq. 0) return
      if (i_time_loop .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_time_loop, i_time_loop)
        if(i_time_loop .gt. 0) exit
!
        call read_character_ctl_item(hd_scheme, i_scheme, scheme_ctl)
        call read_character_ctl_item(hd_diff_correct,                   &
     &        i_diff_correct, diffuse_correct_ctl)
        call read_character_ctl_item(hd_method_4_velo,                  &
     &        i_method_4_velo, method_4_velo_ctl)
        call read_character_ctl_item(hd_precond_4_crank,                &
     &        i_precond_4_crank, precond_4_crank_ctl)
        call read_character_ctl_item(hd_sph_transform_mode,             &
     &          i_sph_transform_mode, sph_transform_mode_ctl)
        call read_character_ctl_item(hd_FFT_package,                    &
     &          i_FFT_package, FFT_library_ctl)
!
        call read_real_ctl_item(hd_eps_4_velo, i_eps_4_velo,            &
     &        eps_4_velo_ctl)
        call read_real_ctl_item(hd_eps_4_magne, i_eps_4_magne,          &
     &        eps_4_magne_ctl)
        call read_real_ctl_item(hd_coef_imp_v, i_coef_imp_v,            &
     &        coef_imp_v_ctl)
        call read_real_ctl_item(hd_coef_imp_t, i_coef_imp_t,            &
     &        coef_imp_t_ctl)
        call read_real_ctl_item(hd_coef_imp_b, i_coef_imp_b,            &
     &        coef_imp_b_ctl)
        call read_real_ctl_item(hd_coef_imp_c, i_coef_imp_c,            &
     &        coef_imp_c_ctl)
        call read_real_ctl_item(hd_eps_crank, i_eps_crank,              &
     &        eps_crank_ctl)
!
        call read_integer_ctl_item(hd_iflag_supg, i_iflag_supg,         &
     &        iflag_supg_ctl)
        call read_integer_ctl_item(hd_num_multi_pass, i_num_multi_pass, &
     &        num_multi_pass_ctl)
        call read_integer_ctl_item(hd_maxiter, i_maxiter, maxiter_ctl)
      end do
!
      end subroutine read_time_loop_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_evo_scheme
