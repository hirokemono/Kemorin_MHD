!
!     module open_sgs_model_coefs
!
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine open_SGS_model_coef_file(iflag_type, id_file,        &
!!     &          file_name, wk_sgs)
!!      subroutine open_SGS_correlation_file(iflag_type, id_file,       &
!!     &          file_name, cd_prop, SGS_param, wk_sgs)
!!      subroutine open_SGS_rms_ratio_file(iflag_type, id_file,         &
!!     &          file_name, cd_prop, SGS_param, wk_sgs)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!
!!      subroutine open_SGS_diff_coef_file(iflag_type, id_file,         &
!!     &          file_name, wk_diff)
!!      subroutine open_diff_correlation_file(iflag_type, id_file,      &
!!     &          file_name, cd_prop, wk_diff)
!!      subroutine open_diff_rms_ratio_file(iflag_type, id_file,        &
!!     &          file_name, cd_prop, wk_diff)
!!        type(conductive_property), intent(in) :: cd_prop
!
      module open_sgs_model_coefs
!
      use m_precision
!
      use t_physical_property
      use t_ele_info_4_dynamic
!
      implicit none
!
      integer(kind = kint), parameter :: iflag_layered = 1
      integer(kind = kint), parameter :: iflag_whole = 2
!
      private :: write_sgs_coef_head
      private :: write_sgs_comps_head, write_diff_comps_head
      private :: write_sgs_whole_time_head
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine open_SGS_model_coef_file(iflag_type, id_file,          &
     &          file_name, wk_sgs)
!
      integer(kind=kint), intent(in) :: id_file, iflag_type
      character(len=kchara), intent(in) :: file_name
!
      type(dynamic_model_data), intent(in) :: wk_sgs
!
!
      open (id_file,file=file_name, status='old',                       &
     &    position='append', err = 99)
      return
!
  99  continue
      open (id_file,file=file_name, status='replace')
!
      if(iflag_type .eq. iflag_layered) then
        call write_sgs_time_head(id_file)
      else if(iflag_type .eq. iflag_whole) then
        call write_sgs_whole_time_head(id_file)
      end if
!
      call write_sgs_coef_head(id_file, wk_sgs)
!
      end subroutine open_SGS_model_coef_file
!
!-----------------------------------------------------------------------
!
      subroutine open_SGS_correlation_file(iflag_type, id_file,         &
     &          file_name, cd_prop, SGS_param, wk_sgs)
!
      use t_SGS_control_parameter
!
      integer(kind=kint), intent(in) :: id_file, iflag_type
      character(len=kchara), intent(in) :: file_name
!
      type(conductive_property), intent(in) :: cd_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(dynamic_model_data), intent(in) :: wk_sgs
!
!
      open (id_file,file=file_name, status='old',                       &
     &    position='append', err = 99)
      return
!
  99  continue
      open (id_file,file=file_name, status='replace')
!
      if(iflag_type .eq. iflag_layered) then
        call write_sgs_time_head(id_file)
      else if(iflag_type .eq. iflag_whole) then
        call write_sgs_whole_time_head(id_file)
      end if
!
      call write_sgs_comps_head                                         &
     &   (id_file, SGS_param%icoord_Csim, cd_prop, wk_sgs)
!
      end subroutine open_SGS_correlation_file
!
!-----------------------------------------------------------------------
!
      subroutine open_SGS_rms_ratio_file(iflag_type, id_file,           &
     &          file_name, cd_prop, SGS_param, wk_sgs)
!
      use t_SGS_control_parameter
!
      integer(kind=kint), intent(in) :: id_file, iflag_type
      character(len=kchara), intent(in) :: file_name
!
      type(conductive_property), intent(in) :: cd_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(dynamic_model_data), intent(in) :: wk_sgs
!
!
      open (id_file,file=file_name, status='old',                       &
     &    position='append', err = 99)
      return
!
  99  continue
      open (id_file,file=file_name, status='replace')
!
      if(iflag_type .eq. iflag_layered) then
        call write_sgs_time_head(id_file)
      else if(iflag_type .eq. iflag_whole) then
        call write_sgs_whole_time_head(id_file)
      end if
!
      call write_sgs_comps_head                                         &
     &    (id_file, SGS_param%icoord_Csim, cd_prop, wk_sgs)
!
      end subroutine open_SGS_rms_ratio_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine open_SGS_diff_coef_file(iflag_type, id_file,           &
     &          file_name, wk_diff)
!
      integer(kind=kint), intent(in) :: id_file, iflag_type
      character(len=kchara), intent(in) :: file_name
!
      type(dynamic_model_data), intent(in) :: wk_diff
!
!
      open (id_file,file=file_name, status='old',                       &
     &    position='append', err = 99)
      return
!
  99  continue
      open (id_file,file=file_name, status='replace')
!
      if(iflag_type .eq. iflag_layered) then
        call write_sgs_time_head(id_file)
      else if(iflag_type .eq. iflag_whole) then
        call write_sgs_whole_time_head(id_file)
      end if
!
      call write_sgs_coef_head(id_file, wk_diff)
!
      end subroutine open_SGS_diff_coef_file
!
!-----------------------------------------------------------------------
!
      subroutine open_diff_correlation_file(iflag_type, id_file,        &
     &          file_name, cd_prop, wk_diff)
!
      integer(kind=kint), intent(in) :: id_file, iflag_type
      character(len=kchara), intent(in) :: file_name
!
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_diff
!
!
      open (id_file,file=file_name, status='old',                       &
     &    position='append', err = 99)
      return
!
  99  continue
      open (id_file,file=file_name, status='replace')
!
      if(iflag_type .eq. iflag_layered) then
        call write_sgs_time_head(id_file)
      else if(iflag_type .eq. iflag_whole) then
        call write_sgs_whole_time_head(id_file)
      end if
!
      call write_diff_comps_head(id_file, cd_prop, wk_diff)
!
      end subroutine open_diff_correlation_file
!
!-----------------------------------------------------------------------
!
      subroutine open_diff_rms_ratio_file(iflag_type, id_file,          &
     &          file_name, cd_prop, wk_diff)
!
      integer(kind=kint), intent(in) :: id_file, iflag_type
      character(len=kchara), intent(in) :: file_name
!
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_diff
!
      open (id_file,file=file_name, status='old',                       &
     &    position='append', err = 99)
      return
!
  99  continue
      open (id_file,file=file_name, status='replace')
!
      if(iflag_type .eq. iflag_layered) then
        call write_sgs_time_head(id_file)
      else if(iflag_type .eq. iflag_whole) then
        call write_sgs_whole_time_head(id_file)
      end if
!
      call write_diff_comps_head(id_file, cd_prop, wk_diff)
!
      end subroutine open_diff_rms_ratio_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_sgs_time_head(file_id)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: file_id
      character(len=kchara) :: label
!
!
      write(label,'(a)') 't_step'
      call write_one_label(file_id, label)
      write(label,'(a)') 'time'
      call write_one_label(file_id, label)
      write(label,'(a)') 'num_layer'
      call write_one_label(file_id, label)
!
      end subroutine write_sgs_time_head
!
!-----------------------------------------------------------------------
!
      subroutine write_sgs_whole_time_head(file_id)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: file_id
      character(len=kchara) :: label
!
      write(label,'(a)') 't_step'
      call write_one_label(file_id, label)
      write(label,'(a)') 'time'
      call write_one_label(file_id, label)
!
      end subroutine write_sgs_whole_time_head
!
!-----------------------------------------------------------------------
!
      subroutine write_sgs_coef_head(file_id, wk_sgs)
!
      use m_phys_labels
      use write_field_labels
!
      integer(kind = kint), intent(in) :: file_id
      type(dynamic_model_data), intent(in) :: wk_sgs
      integer ( kind=kint) :: i
!
!
      do i = 1, wk_sgs%num_kinds
        call write_one_label(file_id, wk_sgs%name(i))
      end do
      write(file_id,*)
!
      end subroutine write_sgs_coef_head
!
!-----------------------------------------------------------------------
!
      subroutine write_sgs_comps_head                                   &
     &         (file_id, icoord_Csim, cd_prop, wk_sgs)
!
      use m_geometry_constants
      use m_phys_labels
!
      use add_direction_labels
      use write_field_labels
      use sel_comp_labels_by_coord
!
      integer(kind = kint), intent(in) :: icoord_Csim
      integer(kind = kint), intent(in) :: file_id
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_sgs
!
      integer ( kind=kint) :: i
      character(len=kchara) :: lab(6), label
!
!
      do i = 1, wk_sgs%num_kinds
        if     (wk_sgs%name(i) .eq. SGS_heat_flux%name) then
          call sel_coord_vector_comp_labels(icoord_Csim,                &
     &        SGS_heat_flux%name, lab(1) )
          call write_vector_label(file_id, lab(1))
!
        else if(wk_sgs%name(i) .eq. SGS_momentum_flux%name) then
          call sel_coord_tensor_comp_labels(icoord_Csim,                &
     &        SGS_momentum_flux%name, lab(1) )
          call write_sym_tensor_label(file_id, lab(1))
!
        else if(wk_sgs%name(i) .eq. SGS_maxwell_tensor%name) then
          call sel_coord_tensor_comp_labels(icoord_Csim,                &
     &        SGS_maxwell_tensor%name, lab(1) )
          call write_sym_tensor_label(file_id, lab(1))
!
        else if(wk_sgs%name(i) .eq. SGS_induction%name) then
          if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
            write(label,'(a)') 'SGS_uxB'
            call sel_coord_vector_comp_labels(icoord_Csim,              &
     &          label, lab(1) )
            call write_vector_label(file_id, lab(1))
!
          else
            write(label,'(a)') 'SGS_induction'
            call sel_coord_tensor_comp_labels(icoord_Csim,              &
     &          label, lab(1) )
            call write_sym_tensor_label(file_id, lab(1))
          end if
!
        else if(wk_sgs%name(i) .eq. SGS_buoyancy%name) then
          call sel_coord_tensor_comp_labels(icoord_Csim,                &
     &        SGS_buoyancy%name, lab(1) )
          call write_sym_tensor_label(file_id, lab(1))
!
        else if(wk_sgs%name(i) .eq. SGS_composit_buoyancy%name) then
          call sel_coord_tensor_comp_labels(icoord_Csim,                &
     &          SGS_composit_buoyancy%name, lab(1) )
          call write_sym_tensor_label(file_id, lab(1))
!
        else if(wk_sgs%name(i) .eq. SGS_composit_flux%name) then
          call sel_coord_vector_comp_labels(icoord_Csim,                &
     &        SGS_composit_flux%name, lab(1) )
          call write_vector_label(file_id, lab(1))
!
        end if
      end do
      write(file_id,*)
!
      end subroutine write_sgs_comps_head
!
!-----------------------------------------------------------------------
!
      subroutine write_diff_comps_head(file_id, cd_prop, wk_diff)
!
      use m_phys_labels
!
      integer(kind = kint), intent(in) :: file_id
      type(conductive_property), intent(in) :: cd_prop
      type(dynamic_model_data), intent(in) :: wk_diff
!
      integer ( kind=kint) :: i
!
!
      do i = 1, wk_diff%num_kinds
        if ( wk_diff%name(i) .eq. velocity%name ) then
          write(file_id,'(a)') 'dVx_dx, dVx_dy, dVx_dz, '
          write(file_id,'(a)') 'dVy_dx, dVy_dy, dVy_dz, '
          write(file_id,'(a)') 'dVz_dx, dVz_dy, dVz_dz, '
        else if ( wk_diff%name(i) .eq. temperature%name ) then
          write(file_id,'(a)') 'dT_dx, dT_dy, dT_dz, '
        else if ( wk_diff%name(i) .eq. magnetic_field%name ) then
          write(file_id,'(a)') 'dBx_dx, dBx_dy, dBx_dz, '
          write(file_id,'(a)') 'dBy_dx, dBy_dy, dBy_dz, '
          write(file_id,'(a)') 'dBz_dx, dBz_dy, dBz_dz, '
        else if ( wk_diff%name(i) .eq. composition%name ) then
          write(file_id,'(a)')                                          &
     &         'composition_x, composition_y, composition_z, '
        else if ( wk_diff%name(i) .eq. SGS_heat_flux%name ) then
          write(file_id,'(a)') 'SGS_hf_x, SGS_hf_y, SGS_hf_z, '
        else if ( wk_diff%name(i) .eq. SGS_momentum_flux%name ) then
          write(file_id,'(a)') 'SGS_mf_xx, SGS_mf_xy, SGS_mf_xz, '
          write(file_id,'(a)') 'SGS_mf_yx, SGS_mf_yy, SGS_mf_yz, '
          write(file_id,'(a)') 'SGS_mf_zx, SGS_mf_yz, SGS_mf_zz, '
        else if ( wk_diff%name(i) .eq. SGS_composit_flux%name ) then
          write(file_id,'(a)') 'SGS_cf_x, SGS_cf_y, SGS_cf_z, '
        else if ( wk_diff%name(i) .eq. SGS_maxwell_tensor%name ) then
          write(file_id,'(a)')                                          &
     &        'SGS_maxwell_xx, SGS_maxwell_xy, SGS_maxwell_xz, '
          write(file_id,'(a)')                                          &
     &        'SGS_maxwell_yx, SGS_maxwell_yy, SGS_maxwell_yz, '
          write(file_id,'(a)')                                          &
     &        'SGS_maxwell_zx, SGS_maxwell_yz, SGS_maxwell_zz, '
        else if ( wk_diff%name(i) .eq. SGS_Lorentz%name) then
          write(file_id,'(a)') 'SGS_lor_xx, SGS_lor_xy, SGS_lor_xz, '
          write(file_id,'(a)') 'SGS_lor_yx, SGS_lor_yy, SGS_lor_yz, '
          write(file_id,'(a)') 'SGS_lor_zx, SGS_lor_yz, SGS_lor_zz, '
        else if ( wk_diff%name(i) .eq. SGS_induction%name ) then
          if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
            write(file_id,'(a)')                                        &
     &        'SGS_uxB_x, SGS_uxB_y, SGS_uxB_z, '
          else
            write(file_id,'(a)')                                        &
     &        'SGS_induction_xy, SGS_induction_xz, SGS_induction_yz, '
          end if
        end if
      end do
!
      end subroutine write_diff_comps_head
!
!-----------------------------------------------------------------------
!
      end module open_sgs_model_coefs
