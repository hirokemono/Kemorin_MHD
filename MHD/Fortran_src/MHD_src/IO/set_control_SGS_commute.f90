!>@file   set_control_SGS_commute.f90
!!@brief  module set_control_SGS_commute
!!
!!@author H. Matsui
!!@date Programmed in 2003
!!@n    modified in Aug., 2007
!!@n    modified in Nov., 2009
!
!> @brief set parameters for SGS model
!!        from control data
!!
!!@verbatim
!!      subroutine s_set_control_SGS_commute                            &
!!     &         (SGS_param, sgs_ctl, cmt_param, Cdiff_file_IO)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        type(commutation_control_params), intent(inout) :: cmt_param
!!        type(field_IO_params), intent(inout) :: Csim_file_IO
!!        type(field_IO_params), intent(inout) :: Cdiff_file_IO
!!        type(SGS_filtering_params), intent(inout) :: filter_param
!!@endverbatim
!
      module set_control_SGS_commute
!
      use m_precision
      use m_error_IDs
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use t_file_IO_parameter
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                      :: def_rst_comm_coef = 'rst_diff_coefs'
      private :: def_rst_comm_coef
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_SGS_commute                              &
     &         (SGS_param, sgs_ctl, cmt_param, Cdiff_file_IO)
!
      use m_phys_labels
      use m_file_format_switch
      use t_ctl_data_SGS_model
      use t_SGS_control_parameter
      use t_field_data_IO
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_model_control), intent(in) :: sgs_ctl
      type(commutation_control_params), intent(inout) :: cmt_param
      type(field_IO_params), intent(inout) :: Cdiff_file_IO
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
      if (SGS_param%iflag_SGS .eq. id_SGS_none) return
!
      if (sgs_ctl%commutate_fld_ctl%icou .gt. 0) then
        do i = 1, sgs_ctl%commutate_fld_ctl%num
          tmpchara = sgs_ctl%commutate_fld_ctl%c_tbl(i)
          if(     tmpchara .eq. fhd_temp) then
            cmt_param%iflag_c_temp =      id_SGS_commute_ON
          else if(tmpchara .eq. fhd_velo) then
            cmt_param%iflag_c_velo =      id_SGS_commute_ON
          else if(tmpchara .eq. fhd_magne) then
            cmt_param%iflag_c_magne =     id_SGS_commute_ON
          else if(tmpchara .eq. fhd_vecp) then
            cmt_param%iflag_c_magne =     id_SGS_commute_ON
          else if(tmpchara .eq. fhd_light) then
            cmt_param%iflag_c_cf =    id_SGS_commute_ON
!
          else if(tmpchara .eq. thd_heat_flux) then
            cmt_param%iflag_c_hf =   id_SGS_commute_ON
          else if(tmpchara .eq. thd_advection) then
            cmt_param%iflag_c_mf =   id_SGS_commute_ON
          else if(tmpchara .eq. thd_lorentz) then
            cmt_param%iflag_c_lorentz = id_SGS_commute_ON
          else if(tmpchara .eq. thd_induction) then
            cmt_param%iflag_c_uxb = id_SGS_commute_ON
          else if(tmpchara .eq. thd_comp_flux) then
            cmt_param%iflag_c_light =  id_SGS_commute_ON
          end if
        end do
!
!
        cmt_param%iflag_c_linear                                        &
     &        =  cmt_param%iflag_c_temp +  cmt_param%iflag_c_velo       &
     &         + cmt_param%iflag_c_magne + cmt_param%iflag_c_light
        cmt_param%iflag_c_nonlinars                                     &
     &        =  cmt_param%iflag_c_hf                                   &
     &         + cmt_param%iflag_c_mf + cmt_param%iflag_c_lorentz       &
     &         + cmt_param%iflag_c_uxb + cmt_param%iflag_c_cf
        cmt_param%iflag_commute = cmt_param%iflag_c_linear              &
     &                              + cmt_param%iflag_c_nonlinars
      end if
!
      if (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        cmt_param%iset_DIFF_coefs = 0
        if (sgs_ctl%DIFF_model_coef_ctl%iflag .ne. 0) then
          tmpchara = sgs_ctl%DIFF_model_coef_ctl%charavalue
!
          if (cmp_no_case(tmpchara, 'whole_domain')) then
            cmt_param%iset_DIFF_coefs = 0
          else if (cmp_no_case(tmpchara, 'layerd')) then
            cmt_param%iset_DIFF_coefs = 1
          end if
        end if
      end if
!
      if (iflag_debug .gt. 0)  then
        write(*,*) 'iflag_commute_temp:     ', cmt_param%iflag_c_temp
        write(*,*) 'iflag_commute_velo:     ', cmt_param%iflag_c_velo
        write(*,*) 'iflag_commute_magne:    ',                          &
     &              cmt_param%iflag_c_magne
        write(*,*) 'iflag_commute_composit: ',                          &
     &              cmt_param%iflag_c_light
        write(*,*) 'iflag_commute_heat:     ', cmt_param%iflag_c_hf
        write(*,*) 'iflag_commute_inertia:  ', cmt_param%iflag_c_mf
        write(*,*) 'iflag_commute_lorentz:  ',                          &
     &              cmt_param%iflag_c_lorentz
        write(*,*) 'iflag_commute_induction:', cmt_param%iflag_c_uxb
        write(*,*) 'iflag_commute_c_flux:   ', cmt_param%iflag_c_cf
!
        write(*,*) 'iset_DIFF_coefs:   ', cmt_param%iset_DIFF_coefs
      end if
!
!
      if(cmt_param%iflag_commute .ne. id_SGS_commute_OFF) then
        cmt_param%iflag_rst_sgs_comm_code                               &
     &        = sgs_ctl%ffile_ctl%commute_coef_ini_head_ctl%iflag
        if(cmt_param%iflag_rst_sgs_comm_code .gt. 0) then
          Cdiff_file_IO%file_prefix                                     &
     &        = sgs_ctl%ffile_ctl%commute_coef_ini_head_ctl%charavalue
        else
          Cdiff_file_IO%file_prefix = def_rst_comm_coef
        end if
!
        call choose_para_file_format                                    &
     &     (sgs_ctl%ffile_ctl%commute_coef_rst_format,                  &
     &      Cdiff_file_IO%iflag_format)
!
        if (iflag_debug .gt. 0)  then
          write(*,*) 'Cdiff_file_IO%file_prefix: ',                     &
     &              trim(Cdiff_file_IO%file_prefix)
          write(*,*) 'Cdiff_file_IO%iflag_format: ',                    &
     &              Cdiff_file_IO%iflag_format
        end if
      end if
!
      end subroutine s_set_control_SGS_commute
!
! -----------------------------------------------------------------------
!
      end module set_control_SGS_commute
