!>@file   m_FEM_sgs_vol_mdl_coefs_IO.f90
!!        module m_FEM_sgs_vol_mdl_coefs_IO
!!
!!@author H. Matsui
!!@date   Programmed in 2005
!>        modified in Aug., 2007
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine output_whole_model_coefs_file                        &
!!     &         (i_step, time, SGS_param, cd_prop, wk_sgs)
!!        integer(kind=kint), intent(in) :: i_step
!!        real(kind=kreal), intent(in) :: time
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!      subroutine output_whole_diff_coefs_file                         &
!!     &         (i_step, time, cd_prop, wk_diff)
!!        integer(kind=kint), intent(in) :: i_step
!!        real(kind=kreal), intent(in) :: time
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(dynamic_model_data), intent(in) :: wk_diff
!!
!!      subroutine read_sgs_whole_data(file_id, iflag, n_comp, coef)
!!        integer(kind = kint), intent(in) :: file_id, iflag, n_comp
!!        real(kind = kreal), intent(inout) :: coef(n_comp)
!!      subroutine read_sgs_whole_2data(file_id, iflag, n_comp,         &
!!     &                                coef1, coef2)
!!        integer(kind = kint), intent(in) :: file_id, iflag, n_comp
!!        real(kind = kreal), intent(inout) :: coef1(n_comp)
!!        real(kind = kreal), intent(inout) :: coef2(n_comp)
!!@endverbatim
!!
      module m_FEM_sgs_vol_mdl_coefs_IO
!
      use m_precision
!
      use calypso_mpi
      use t_physical_property
      use t_SGS_control_parameter
      use t_work_FEM_dynamic_SGS
      use open_sgs_model_coefs
!
      implicit none
!
      integer(kind=kint), parameter, private :: id_sgs_file_code = 29
!
      character(len=kchara), parameter, private                         &
     &     :: sgs_fld_whole_file_name =  'sgs_m_coefs_whole.dat'
      character(len=kchara), parameter, private                         &
     &     :: sgs_comp_whole_file_name = 'sgs_m_coefs_whole_comp.dat'
      character(len=kchara), parameter, private                         &
     &     :: diff_fld_whole_file_name = 'diff_m_coefs_whole.dat'
      character(len=kchara), parameter, private                         &
     &     :: diff_comp_whole_file_name = 'diff_m_comp_coefs_whole.dat'
!
      character(len=kchara), parameter, private                         &
     &       :: sgs_w_cor_file_name =    'sgs_correlate_whole.dat'
      character(len=kchara), parameter, private                         &
     &       :: sgs_w_cov_file_name =    'sgs_covariance_whole.dat'
      character(len=kchara), parameter, private                         &
     &       :: sgs_w_ratio_file_name =  'sgs_rms_ratio_whole.dat'
      character(len=kchara), parameter, private                         &
     &       :: sgs_w_rms_file_name =    'sgs_rms_whole.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_w_cor_file_name =   'diff_correlate_whole.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_w_cov_file_name =   'diff_covariance_whole.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_w_ratio_file_name = 'diff_rms_ratio_whole.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_w_rms_file_name =   'diff_rms_whole.dat'
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine output_whole_model_coefs_file                          &
     &         (i_step, time, SGS_param, cd_prop, wk_sgs)
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: time
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_sgs
!
!
      call open_SGS_model_coef_file(iflag_whole,                        &
     &    id_sgs_file_code, sgs_fld_whole_file_name, wk_sgs)
      write(id_sgs_file_code,1001)  i_step, time,                       &
     &        wk_sgs%fld_whole_clip(1:wk_sgs%num_kinds)
      close (id_sgs_file_code)
!
      call open_SGS_correlation_file(iflag_whole,                       &
     &    id_sgs_file_code, sgs_comp_whole_file_name,                   &
     &    cd_prop, SGS_param, wk_sgs)
      write(id_sgs_file_code,1001)  i_step, time,                       &
     &        wk_sgs%comp_whole_clip(1:wk_sgs%ntot_comp)
      close (id_sgs_file_code)
!
      call open_SGS_correlation_file(iflag_whole,                       &
     &    id_sgs_file_code, sgs_w_cor_file_name,                        &
     &    cd_prop, SGS_param, wk_sgs)
      write(id_sgs_file_code,1001)  i_step, time,                       &
     &        wk_sgs%corrilate_w(1:wk_sgs%ntot_comp)
      close (id_sgs_file_code)
!
      call open_SGS_correlation_file(iflag_whole,                       &
     &    id_sgs_file_code, sgs_w_cov_file_name,                        &
     &    cd_prop, SGS_param, wk_sgs)
      write(id_sgs_file_code,1001)  i_step, time,                       &
     &        wk_sgs%covariant_w(1:wk_sgs%ntot_comp)
      close (id_sgs_file_code)
!
      call open_SGS_correlation_file(iflag_whole,                       &
     &    id_sgs_file_code, sgs_w_ratio_file_name,                      &
     &    cd_prop, SGS_param, wk_sgs)
      write(id_sgs_file_code,1001) i_step, time,                        &
     &        wk_sgs%ratio_w(1:wk_sgs%ntot_comp)
      close (id_sgs_file_code)
!
      call open_SGS_rms_ratio_file(iflag_whole,                         &
     &    id_sgs_file_code, sgs_w_rms_file_name,                        &
     &    cd_prop, SGS_param, wk_sgs)
      write(id_sgs_file_code,1001) i_step, time,                        &
     &        wk_sgs%rms_simi_w(1:wk_sgs%ntot_comp),                    &
     &        wk_sgs%rms_grad_w(1:wk_sgs%ntot_comp)
      close (id_sgs_file_code)
!
 1001 format(i16,1pE25.15e3,1p200E25.15e3)
!
      end subroutine output_whole_model_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine output_whole_diff_coefs_file                           &
     &         (i_step, time, cd_prop, wk_diff)
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: time
!
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_diff
!
!
      call open_SGS_diff_coef_file(iflag_whole,                         &
     &      id_sgs_file_code, diff_fld_whole_file_name, wk_diff)
      write(id_sgs_file_code,1001) i_step, time,                        &
     &          wk_diff%fld_whole_clip(1:wk_diff%num_kinds)
      close (id_sgs_file_code)
!
      call open_SGS_diff_coef_file(iflag_whole,                         &
     &      id_sgs_file_code, diff_comp_whole_file_name, wk_diff)
      write(id_sgs_file_code,1001) i_step, time,                        &
     &          wk_diff%comp_whole_clip(1:wk_diff%ntot_comp)
      close (id_sgs_file_code)
!
      call open_diff_correlation_file(iflag_whole,                      &
     &   id_sgs_file_code, diff_w_cor_file_name, cd_prop, wk_diff)
      write(id_sgs_file_code,1001) i_step, time,                        &
     &          wk_diff%corrilate_w(1:wk_diff%ntot_comp)
      close (id_sgs_file_code)
!
      call open_diff_correlation_file(iflag_whole,                      &
     &   id_sgs_file_code, diff_w_cov_file_name, cd_prop, wk_diff)
      write(id_sgs_file_code,1001) i_step, time,                        &
     &          wk_diff%covariant_w(1:wk_diff%ntot_comp)
      close (id_sgs_file_code)
!
      call open_diff_correlation_file(iflag_whole,                      &
     &   id_sgs_file_code, diff_w_ratio_file_name,                      &
     &   cd_prop, wk_diff)
      write(id_sgs_file_code,1001) i_step, time,                        &
     &          wk_diff%ratio_w(1:wk_diff%ntot_comp)
      close (id_sgs_file_code)
!
      call open_diff_rms_ratio_file(iflag_whole,                        &
     &    id_sgs_file_code, diff_w_rms_file_name, cd_prop, wk_diff)
      write(id_sgs_file_code,1001) i_step, time,                        &
     &          wk_diff%rms_simi_w(1:wk_diff%ntot_comp),                &
     &          wk_diff%rms_grad_w(1:wk_diff%ntot_comp)
      close (id_sgs_file_code)
!
 1001 format(i16,1pE25.15e3,1p200E25.15e3)
!
      end subroutine output_whole_diff_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine read_sgs_whole_data(file_id, iflag, n_comp, coef)
!
      integer(kind = kint), intent(in) :: file_id, iflag, n_comp
      real(kind = kreal), intent(inout) :: coef(n_comp)
!
      integer(kind = kint) :: nd, i_read_step
      real(kind = kreal) :: rtmp
!
!
      do
          read(file_id,*,err=99,end=99)                                 &
     &        i_read_step, rtmp, (coef(nd),nd=1, n_comp)
        if (i_read_step .ge. iflag) exit
      end do
 99   continue
!
      end subroutine read_sgs_whole_data
!
!-----------------------------------------------------------------------
!
      subroutine read_sgs_whole_2data(file_id, iflag, n_comp,           &
     &                                coef1, coef2)
!
      integer(kind = kint), intent(in) :: file_id, iflag, n_comp
      real(kind = kreal), intent(inout) :: coef1(n_comp)
      real(kind = kreal), intent(inout) :: coef2(n_comp)
!
      integer(kind = kint) :: nd, i_read_step
      real(kind = kreal) :: rtmp
!
!
      do
          read(file_id,*,err=99,end=99) i_read_step, rtmp,              &
     &     (coef1(nd),nd=1, n_comp), (coef2(nd),nd=1, n_comp)
        if (i_read_step .ge. iflag) exit
      end do
 99   continue
!
      end subroutine read_sgs_whole_2data
!
!-----------------------------------------------------------------------
!
      end module m_FEM_sgs_vol_mdl_coefs_IO
