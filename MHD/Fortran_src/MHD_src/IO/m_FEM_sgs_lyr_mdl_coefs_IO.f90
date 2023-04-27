!>@file   m_FEM_sgs_lyr_mdl_coefs_IO.f90
!!        module m_FEM_sgs_lyr_mdl_coefs_IO
!!
!!@author H. Matsui
!!@date   Programmed in 2005
!>        modified in Aug., 2007
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine output_layered_model_coefs_file                      &
!!     &         (i_step, time, SGS_param, cd_prop, wk_sgs)
!!        integer(kind=kint), intent(in) :: i_step
!!        real(kind=kreal), intent(in) :: time
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!      subroutine output_layered_diff_coefs_file                       &
!!     &         (i_step, time, cd_prop, wk_diff)
!!        integer(kind=kint), intent(in) :: i_step
!!        real(kind=kreal), intent(in) :: time
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(dynamic_model_data), intent(in) :: wk_diff
!!
!!      subroutine read_sgs_layerd_data(file_id, iflag, n_layer,        &
!!     &          n_comp, coef)
!!        integer(kind = kint), intent(in) :: file_id, iflag
!!        integer(kind = kint), intent(in) :: n_layer, n_comp
!!        real(kind = kreal), intent(inout) :: coef(n_layer, n_comp)
!!      subroutine read_sgs_layerd_2data(file_id, iflag, n_layer,       &
!!     &          n_comp, coef1, coef2)
!!        integer(kind = kint), intent(in) :: file_id, iflag
!!        integer(kind = kint), intent(in) :: n_layer, n_comp
!!        real(kind = kreal), intent(inout) :: coef1(n_layer, n_comp)
!!        real(kind = kreal), intent(inout) :: coef2(n_layer, n_comp)
!!@endverbatim
!
      module m_FEM_sgs_lyr_mdl_coefs_IO
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
     &     :: sgs_fld_coef_file_name =  'sgs_model_coefs.dat'
      character(len=kchara), parameter, private                         &
     &     :: sgs_comp_coef_file_name = 'sgs_model_coefs_comp.dat'
      character(len=kchara), parameter, private                         &
     &     :: diff_coef_file_name =    'diff_model_coefs.dat'
!
      character(len=kchara), parameter, private                         &
     &       :: sgs_cor_file_name =      'sgs_correlate.dat'
      character(len=kchara), parameter, private                         &
     &       :: sgs_cov_file_name =      'sgs_covariance.dat'
      character(len=kchara), parameter, private                         &
     &       :: sgs_ratio_file_name =    'sgs_rms_ratio.dat'
      character(len=kchara), parameter, private                         &
     &       :: sgs_rms_file_name =      'sgs_rms.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_cor_file_name =     'diff_correlate.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_cov_file_name =     'diff_covariance.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_ratio_file_name =   'diff_rms_ratio.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_rms_file_name =     'diff_rms.dat'
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine output_layered_model_coefs_file                        &
     &         (i_step, time, SGS_param, cd_prop, wk_sgs)
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: time
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_sgs
!
      integer (kind = kint) :: inum
!
!
      call open_SGS_model_coef_file(iflag_layered,                      &
     &    id_sgs_file_code, sgs_fld_coef_file_name, wk_sgs)
      do inum = 1, wk_sgs%nlayer
        write(id_sgs_file_code,1000) i_step,                            &
     &      time, inum, wk_sgs%fld_clip(inum,1:wk_sgs%num_kinds)
      end do
      close (id_sgs_file_code)
!
      call open_SGS_correlation_file(iflag_layered,                     &
     &    id_sgs_file_code, sgs_comp_coef_file_name,                    &
     &    cd_prop, SGS_param, wk_sgs)
      do inum = 1, wk_sgs%nlayer
        write(id_sgs_file_code,1000) i_step,                            &
     &      time, inum, wk_sgs%comp_clip(inum,1:wk_sgs%ntot_comp)
      end do
      close (id_sgs_file_code)
!
      call open_SGS_correlation_file(iflag_layered,                     &
     &    id_sgs_file_code, sgs_cor_file_name,                          &
     &    cd_prop, SGS_param, wk_sgs)
      do inum = 1, wk_sgs%nlayer
        write(id_sgs_file_code,1000) i_step, time, inum,                &
     &         wk_sgs%corrilate(inum,1:wk_sgs%ntot_comp)
      end do
      close (id_sgs_file_code)
!
      call open_SGS_correlation_file(iflag_layered,                     &
     &    id_sgs_file_code, sgs_cov_file_name,                          &
     &    cd_prop, SGS_param, wk_sgs)
      do inum = 1, wk_sgs%nlayer
        write(id_sgs_file_code,1000) i_step, time, inum,                &
     &         wk_sgs%covariant(inum,1:wk_sgs%ntot_comp)
      end do
      close (id_sgs_file_code)
!
      call open_SGS_correlation_file(iflag_layered,                     &
     &    id_sgs_file_code, sgs_ratio_file_name,                        &
     &    cd_prop, SGS_param, wk_sgs)
      do inum = 1, wk_sgs%nlayer
        write(id_sgs_file_code,1000) i_step, time, inum,                &
     &         wk_sgs%ratio(inum,1:wk_sgs%ntot_comp)
      end do
      close (id_sgs_file_code)
!
      call open_SGS_rms_ratio_file(iflag_layered,                       &
     &    id_sgs_file_code, sgs_rms_file_name,                          &
     &    cd_prop, SGS_param, wk_sgs)
      do inum = 1, wk_sgs%nlayer
        write(id_sgs_file_code,1000) i_step, time, inum,                &
     &         wk_sgs%rms_simi(inum,1:wk_sgs%ntot_comp),                &
     &         wk_sgs%rms_grad(inum,1:wk_sgs%ntot_comp)
      end do
      close (id_sgs_file_code)
!
 1000 format(i16,1pE25.15e3,i16,1p200E25.15e3)
!
      end subroutine output_layered_model_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine output_layered_diff_coefs_file                         &
     &         (i_step, time, cd_prop, wk_diff)
!
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: time
!
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_diff
!
      integer (kind = kint) :: inum
!
!
      call open_SGS_diff_coef_file(iflag_layered,                       &
     &    id_sgs_file_code, diff_coef_file_name, wk_diff)
      do inum = 1, wk_diff%nlayer
        write(id_sgs_file_code,1000) i_step, time, inum,                &
     &              wk_diff%fld_clip(inum,1:wk_diff%num_kinds)
      end do
      close(id_sgs_file_code)
!
      call open_diff_correlation_file(iflag_layered,                    &
     &    id_sgs_file_code, diff_cor_file_name, cd_prop, wk_diff)
      do inum = 1, wk_diff%nlayer
        write(id_sgs_file_code,1000)  i_step, time, inum,               &
     &              wk_diff%corrilate(inum,1:wk_diff%ntot_comp)
      end do
      close(id_sgs_file_code)
!
      call open_diff_correlation_file(iflag_layered,                    &
     &    id_sgs_file_code, diff_cov_file_name, cd_prop, wk_diff)
      do inum = 1, wk_diff%nlayer
        write(id_sgs_file_code,1000)  i_step, time, inum,               &
     &              wk_diff%covariant(inum,1:wk_diff%ntot_comp)
      end do
      close(id_sgs_file_code)
!
      call open_diff_correlation_file(iflag_layered,                    &
     &    id_sgs_file_code, diff_ratio_file_name,                       &
     &    cd_prop, wk_diff)
      do inum = 1, wk_diff%nlayer
        write(id_sgs_file_code,1000) i_step, time, inum,                &
     &              wk_diff%ratio(inum,1:wk_diff%ntot_comp)
      end do
      close(id_sgs_file_code)
!
      call open_diff_rms_ratio_file(iflag_layered,                      &
     &    id_sgs_file_code, diff_rms_file_name, cd_prop, wk_diff)
      do inum = 1, wk_diff%nlayer
        write(id_sgs_file_code,1000) i_step, time, inum,                &
     &              wk_diff%rms_simi(inum,1:wk_diff%ntot_comp),         &
     &              wk_diff%rms_grad(inum,1:wk_diff%ntot_comp)
      end do
      close(id_sgs_file_code)
!
 1000 format(i16,1pE25.15e3,i16,1p200E25.15e3)
!
      end subroutine output_layered_diff_coefs_file
!
!-----------------------------------------------------------------------
!
      subroutine read_sgs_layerd_data(file_id, iflag, n_layer,          &
     &          n_comp, coef)
!
      integer(kind = kint), intent(in) :: file_id, iflag
      integer(kind = kint), intent(in) :: n_layer, n_comp
      real(kind = kreal), intent(inout) :: coef(n_layer, n_comp)
!
      integer(kind = kint) :: inum, nd, i_read_step, itmp
      real(kind = kreal) :: rtmp
!
!
      i_read_step = -1
      do
        do inum = 1, n_layer
          read(file_id,*,err=99,end=99)                                 &
     &        i_read_step, rtmp, itmp, (coef(inum,nd),nd=1, n_comp)
        end do
        if (i_read_step .ge. iflag) exit
      end do
 99   continue
!
      end subroutine read_sgs_layerd_data
!
!-----------------------------------------------------------------------
!
      subroutine read_sgs_layerd_2data(file_id, iflag, n_layer,         &
     &          n_comp, coef1, coef2)
!
      integer(kind = kint), intent(in) :: file_id, iflag
      integer(kind = kint), intent(in) :: n_layer, n_comp
      real(kind = kreal), intent(inout) :: coef1(n_layer, n_comp)
      real(kind = kreal), intent(inout) :: coef2(n_layer, n_comp)
!
      integer(kind = kint) :: inum, nd, i_read_step, itmp
      real(kind = kreal) :: rtmp
!
!
      i_read_step = -1
      do
        do inum = 1, n_layer
          read(file_id,*,err=99,end=99) i_read_step, rtmp, itmp,        &
     &     (coef1(inum,nd),nd=1, n_comp), (coef2(inum,nd),nd=1, n_comp)
        end do
        if (i_read_step .ge. iflag) exit
      end do
 99   continue
!
      end subroutine read_sgs_layerd_2data
!
!-----------------------------------------------------------------------
!
      end module m_FEM_sgs_lyr_mdl_coefs_IO
