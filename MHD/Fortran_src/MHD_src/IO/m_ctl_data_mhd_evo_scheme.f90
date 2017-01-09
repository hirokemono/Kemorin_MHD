!>@file   m_ctl_data_mhd_evo_scheme.f90
!!@brief  module m_ctl_data_mhd_evo_scheme
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine read_restart_control
!!      subroutine read_time_loop_control
!!
!! !!!!  control for initial and restart data  !!!!!!!!!!!!!!!!!!!!!!!!!!
!!   no_data:             No initial values
!!   start_from_rst_file: Read restart data as initial values
!!
!!   dynamo_benchmark_0: Initial values for dynamo benchmark Case 0
!!   dynamo_benchmark_1: Initial values for dynamo benchmark Case 1
!!   dynamo_benchmark_2: Initial values for dynamo benchmark Case 1
!!
!!   pseudo_vacuum_benchmark: Initial values for pseudo vacuum benchmark
!!
!!   rotate_x: rotate around x-axis
!!   rotate_y: rotate around y-axis
!!   rotate_z: rotate around z-axis
!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin restart_file_ctl
!!     rst_ctl                start_from_rst_file
!!    end restart_file_ctl
!!
!! !!!   method for time evolution  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   iflag_supg_ctl:      0...no SUPG 1...SUPG
!!   iflag_supg_v_ctl           Off
!!   iflag_supg_t_ctl           Off
!!   iflag_supg_b_ctl           Off
!!   iflag_supg_c_ctl           Off
!!
!!   num_multi_pass_ctl:  iteration counts for multi pass
!!   maxiter_ctl:         maximum iteration number for correction
!!   eps_4_velo_ctl:      ||div v||_{n} / ||div v||_{n-1}
!!   eps_4_magne_ctl:     ||div B||_{n} / ||div B||_{n-1}
!!   scheme_ctl:          Scheme for time evolution
!!                 explicit_Euler...explicit_Euler
!!                 2nd_Adams_Bashforth...2nd_Adams_Bashforth
!!                 Crank_Nicolson...Crank_Nicolson with 2nd_Adams_Bashforth
!!                 Crank_Nicolson_consist...Crank_Nicolson
!!                                         with consistent mass matrix
!!   eps_crank_ctl:        
!!   method_4_velo_ctl:    method for Crank Nicolson Scheme
!!   precond_4_crank_ctl:  preconditioning method for Crank Nicolson Scheme
!!
!!   Legendre_trans_loop_ctl: Legendre_transform loop type
!!                   ('inner_radial_loop' 'outer_radial_loop' 'long_loop')
!!   FFT_library_ctl:  Selection of FFT librarry  ('FFTW3' or 'FFTPACK')
!!   import_table_mode_ctl:   Selection of import mode
!!                     ('regular_table' or 'reversed_table')
!!   send_recv_routine_ctl:    Selection of send_recv_routines
!!                     ('SEND_RECV', 'AllToAllv', or 'AllToAll')
!!   Legendre_vector_length_ctl:  vector length for Legendre transform
!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    begin time_loop_ctl
!!      iflag_supg_ctl           0
!!      num_multi_pass_ctl       1
!!      maxiter_ctl              1
!!      eps_4_velo_ctl           5.0e-1
!!      eps_4_magne_ctl          5.0e-1
!!      scheme_ctl              Crank_Nicolson
!!      diffuse_correct_ctl     On
!!      coef_imp_v_ctl          5.0e-1
!!      coef_imp_t_ctl          5.0e-1
!!      coef_imp_b_ctl          5.0e-1
!!      coef_imp_c_ctl          5.0e-1
!!
!!      eps_crank_ctl           1.0e-6
!!      eps_B_solver_ctl        1.0e-6
!!      method_4_velo_ctl      CG 
!!      precond_4_crank_ctl     SSOR   
!!
!!      Legendre_trans_loop_ctl   'inner_radial_loop'
!!      FFT_library_ctl           'FFTW'
!!
!!      Legendre_vector_length_ctl    2
!!    end time_loop_ctl
!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_ctl_data_mhd_evo_scheme
!
      use m_precision
      use t_ctl_data_mhd_evo_scheme
!
      implicit  none
!
!
      type(mhd_restart_control), save :: mr_ctl1
      type(mhd_evo_scheme_control), save :: mevo_ctl1
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
!
      private :: hd_restart_file, i_restart_file
      private :: hd_time_loop, i_time_loop
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_restart_control
!
!
      call read_restart_ctl(hd_restart_file, i_restart_file, mr_ctl1)
!
      end subroutine read_restart_control
!
!   --------------------------------------------------------------------
!
      subroutine read_time_loop_control
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      call read_time_loop_ctl(hd_time_loop, i_time_loop, mevo_ctl1)
!
      end subroutine read_time_loop_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_mhd_evo_scheme
