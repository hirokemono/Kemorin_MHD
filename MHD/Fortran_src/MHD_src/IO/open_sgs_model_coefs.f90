!
!     module open_sgs_model_coefs
!
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2007
!
!      subroutine s_open_sgs_model_coefs(my_rank)
!      subroutine close_sgs_model_coefs(my_rank)
!
!      subroutine write_sgs_coef_head(file_id)
!
      module open_sgs_model_coefs
!
      use m_precision
!
      implicit none
!
      private :: write_sgs_comps_head, write_diff_comps_head
      private :: write_sgs_whole_time_head
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_open_sgs_model_coefs(my_rank)
!
       use m_file_control_parameter
       use m_ele_info_4_dynamical
       use m_control_parameter
!
      integer (kind=kint), intent(in) :: my_rank
!
!
      if (my_rank .eq. 0                                                &
     &     .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!
        open (sgs_fld_coef_file_code,file=sgs_fld_coef_file_name,       &
     &      status='replace')
        call write_sgs_time_head(sgs_fld_coef_file_code)
        call write_sgs_coef_head(sgs_fld_coef_file_code)
!
        open (sgs_fld_whole_file_code,file=sgs_fld_whole_file_name,     &
     &      status='replace')
        call write_sgs_whole_time_head(sgs_fld_whole_file_code)
        call write_sgs_coef_head(sgs_fld_whole_file_code)
!
!
        open (sgs_cor_file_code,file=sgs_cor_file_name,                 &
     &      status='replace')
        call write_sgs_time_head(sgs_cor_file_code)
        call write_sgs_comps_head(sgs_cor_file_code)
!
        open (sgs_cov_file_code,file=sgs_cor_file_name,                 &
     &      status='replace')
        call write_sgs_time_head(sgs_cov_file_code)
        call write_sgs_comps_head(sgs_cov_file_code)
!
        open (sgs_ratio_file_code,file=sgs_ratio_file_name,             &
     &      status='replace')
        call write_sgs_time_head(sgs_ratio_file_code)
        call write_sgs_comps_head(sgs_ratio_file_code)
!
        open (sgs_comp_coef_file_code,file=sgs_comp_coef_file_name,     &
     &      status='replace')
        call write_sgs_time_head(sgs_comp_coef_file_code)
        call write_sgs_comps_head(sgs_comp_coef_file_code)
!
        open (sgs_comp_whole_file_code,file=sgs_comp_whole_file_name,   &
     &      status='replace')
        call write_sgs_whole_time_head(sgs_comp_whole_file_code)
        call write_sgs_comps_head(sgs_comp_whole_file_code)
!
        open (sgs_rms_file_code,file=sgs_rms_file_name,                 &
     &      status='replace')
        call write_sgs_time_head(sgs_rms_file_code)
        call write_sgs_comps_head(sgs_rms_file_code)
        call write_sgs_comps_head(sgs_rms_file_code)
!
        open (sgs_w_cor_file_code,file=sgs_w_cor_file_name,             &
     &        status='replace')
        call write_sgs_whole_time_head(sgs_w_cor_file_code)
        call write_sgs_comps_head(sgs_w_cor_file_code)
!
        open (sgs_w_cov_file_code,file=sgs_w_cov_file_name,             &
     &        status='replace')
        call write_sgs_whole_time_head(sgs_w_cov_file_code)
        call write_sgs_comps_head(sgs_w_cov_file_code)
!
        open (sgs_w_ratio_file_code,file=sgs_w_ratio_file_name,         &
     &        status='replace')
        call write_sgs_whole_time_head(sgs_w_ratio_file_code)
        call write_sgs_comps_head(sgs_w_ratio_file_code)
!
        open (sgs_w_rms_file_code,file=sgs_w_rms_file_name,             &
     &        status='replace')
        call write_sgs_whole_time_head(sgs_w_rms_file_code)
        call write_sgs_comps_head(sgs_w_rms_file_code)
        call write_sgs_comps_head(sgs_w_rms_file_code)
!
        open (sgs_diff_max_code,file = sgs_diff_max_name,               &
     &      status='replace')
!
        if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
!
          open (diff_fld_whole_file_code,file=diff_fld_whole_file_name, &
     &      status='replace')
          call write_sgs_whole_time_head(diff_fld_whole_file_code)
          call write_diff_coef_head(diff_fld_whole_file_code)
!
          open (diff_comp_whole_file_code,                              &
     &        file=diff_comp_whole_file_name, status='replace')
          call write_sgs_whole_time_head(diff_comp_whole_file_code)
          call write_sgs_comps_head(diff_comp_whole_file_code)
!
          open (diff_w_cor_file_code,file=diff_w_cor_file_name,         &
     &        status='replace')
          call write_sgs_whole_time_head(diff_w_cor_file_code)
          call write_diff_comps_head(diff_w_cor_file_code)
!
          open (diff_w_cov_file_code,file=diff_w_cov_file_name,         &
     &        status='replace')
          call write_sgs_whole_time_head(diff_w_cov_file_code)
          call write_diff_comps_head(diff_w_cov_file_code)
!
          open (diff_w_ratio_file_code,file=diff_w_ratio_file_name,     &
     &        status='replace')
          call write_sgs_whole_time_head(diff_w_ratio_file_code)
          call write_diff_comps_head(diff_w_ratio_file_code)
!
          open (diff_w_rms_file_code,file=diff_w_rms_file_name,         &
     &        status='replace')
          call write_sgs_whole_time_head(diff_w_rms_file_code)
          call write_diff_comps_head(diff_w_rms_file_code)
          call write_diff_comps_head(diff_w_rms_file_code)
!
!
          if (iset_DIFF_model_coefs .eq. 1) then
!
            open (diff_coef_file_code,file=diff_coef_file_name,         &
     &            status='replace')
            call write_sgs_time_head(diff_coef_file_code)
            call write_diff_coef_head(diff_coef_file_code)
!
            open (diff_cor_file_code,file=diff_cor_file_name,           &
     &          status='replace')
            call write_sgs_time_head(diff_cor_file_code)
            call write_diff_comps_head(diff_cor_file_code)
!
            open (diff_cov_file_code,file=diff_cov_file_name,           &
     &          status='replace')
            call write_sgs_time_head(diff_cov_file_code)
            call write_diff_comps_head(diff_cov_file_code)
!
            open (diff_ratio_file_code,file=diff_ratio_file_name,       &
     &          status='replace')
            call write_sgs_time_head(diff_ratio_file_code)
            call write_diff_comps_head(diff_ratio_file_code)
!
            open (diff_rms_file_code,file=diff_rms_file_name,           &
     &          status='replace')
            call write_sgs_time_head(diff_rms_file_code)
            call write_diff_comps_head(diff_rms_file_code)
            call write_diff_comps_head(diff_rms_file_code)
!
          end if
        end if
!
      end if
!
      end subroutine s_open_sgs_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine close_sgs_model_coefs(my_rank)
!
       use m_file_control_parameter
       use m_control_parameter
!
      integer (kind=kint), intent(in) :: my_rank
!
!
      if (my_rank .eq. 0                                                &
     &      .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
!
        close (sgs_fld_coef_file_code)
        close (sgs_comp_coef_file_code)
        close (sgs_fld_whole_file_code)
        close (sgs_comp_whole_file_code)
!
        close (sgs_cov_file_code)
        close (sgs_cor_file_code)
        close (sgs_ratio_file_code)
        close (sgs_rms_file_code)
        close (sgs_w_cor_file_code)
        close (sgs_w_cov_file_code)
        close (sgs_w_ratio_file_code)
        close (sgs_w_rms_file_code)
!
        close (sgs_diff_max_code)
!
        if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
!
          close (diff_fld_whole_file_code)
          close (diff_comp_whole_file_code)
          close (diff_w_cor_file_code)
          close (diff_w_cov_file_code)
          close (diff_w_ratio_file_code)
          close (diff_w_rms_file_code)
!
          if (iset_DIFF_model_coefs .eq. 1 ) then
            close (diff_coef_file_code)
            close (diff_cor_file_code)
            close (diff_cov_file_code)
            close (diff_ratio_file_code)
            close (diff_rms_file_code)
          end if
        end if
!
      end if
!
      end subroutine close_sgs_model_coefs
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
      subroutine write_sgs_coef_head(file_id)
!
      use m_ele_info_4_dynamical
      use m_phys_labels
      use m_SGS_model_coefs
      use write_field_labels
!
      integer(kind = kint), intent(in) :: file_id
      integer ( kind=kint) :: i
!
!
      do i = 1, num_sgs_kinds
        if ( name_ak_sgs(i) .eq. fhd_SGS_h_flux ) then
          call write_one_label(file_id, fhd_SGS_h_flux)
        else if ( name_ak_sgs(i) .eq. fhd_SGS_m_flux ) then
          call write_one_label(file_id, fhd_SGS_m_flux)
        else if ( name_ak_sgs(i) .eq. fhd_SGS_maxwell_t ) then
          call write_one_label(file_id, fhd_SGS_maxwell_t)
        else if ( name_ak_sgs(i) .eq. fhd_SGS_induction ) then
          call write_one_label(file_id, fhd_SGS_induction)
        else if ( name_ak_sgs(i) .eq. fhd_SGS_c_flux ) then
          call write_one_label(file_id, fhd_SGS_c_flux)
        else if ( name_ak_sgs(i) .eq. fhd_SGS_buoyancy ) then
          call write_one_label(file_id, fhd_SGS_buoyancy)
        else if ( name_ak_sgs(i) .eq. fhd_SGS_comp_buo ) then
          call write_one_label(file_id, fhd_SGS_comp_buo)
        end if
      end do
      write(file_id,*)
!
      end subroutine write_sgs_coef_head
!
!-----------------------------------------------------------------------
!
      subroutine write_diff_coef_head(file_id)
!
      use m_ele_info_4_dynamical
      use m_phys_labels
      use m_SGS_model_coefs
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: file_id
      integer ( kind=kint) :: i
!
!
      do i = 1, num_diff_kinds
        if ( name_ak_diff(i) .eq. fhd_velo ) then
          call write_one_label(file_id, fhd_velo)
        else if ( name_ak_diff(i) .eq. fhd_temp ) then
          call write_one_label(file_id, fhd_temp)
        else if ( name_ak_diff(i) .eq. fhd_magne ) then
          call write_one_label(file_id, fhd_magne)
        else if ( name_ak_diff(i) .eq. fhd_light ) then
          call write_one_label(file_id, fhd_light)
        else if ( name_ak_diff(i) .eq. fhd_SGS_h_flux ) then
          call write_one_label(file_id, fhd_SGS_h_flux)
        else if ( name_ak_diff(i) .eq. fhd_SGS_m_flux ) then
          call write_one_label(file_id, fhd_SGS_m_flux)
        else if ( name_ak_sgs(i) .eq. fhd_SGS_maxwell_t ) then
          call write_one_label(file_id, fhd_SGS_maxwell_t)
        else if ( name_ak_sgs(i) .eq. fhd_SGS_induction ) then
          call write_one_label(file_id, fhd_SGS_induction)
        else if ( name_ak_diff(i) .eq. fhd_SGS_c_flux ) then
          call write_one_label(file_id, fhd_SGS_c_flux)
        end if
      end do
      write(file_id,*)
!
      end subroutine write_diff_coef_head
!
!-----------------------------------------------------------------------
!
      subroutine write_sgs_comps_head(file_id)
!
      use m_control_parameter
      use m_geometry_constants
      use m_ele_info_4_dynamical
      use m_phys_labels
      use m_SGS_model_coefs
!
      use add_direction_labels
      use write_field_labels
      use sel_comp_labels_by_coord
!
      integer(kind = kint), intent(in) :: file_id
      integer ( kind=kint) :: i
      character(len=kchara) :: lab(6), label
!
!
      do i = 1, num_sgs_kinds
        if ( name_ak_sgs(i) .eq. fhd_SGS_h_flux ) then
          call sel_coord_vector_comp_labels(icoord_SGS_model_coef,      &
     &        fhd_SGS_h_flux, lab(1) )
          call write_vector_label(file_id, lab(1))
!
        else if ( name_ak_sgs(i) .eq. fhd_SGS_m_flux ) then
          call sel_coord_tensor_comp_labels(icoord_SGS_model_coef,      &
     &        fhd_SGS_m_flux, lab(1) )
          call write_sym_tensor_label(file_id, lab(1))
!
        else if ( name_ak_sgs(i) .eq. fhd_SGS_maxwell_t ) then
          call sel_coord_tensor_comp_labels(icoord_SGS_model_coef,      &
     &        fhd_SGS_maxwell_t, lab(1) )
          call write_sym_tensor_label(file_id, lab(1))
!
        else if ( name_ak_sgs(i) .eq. fhd_SGS_induction ) then
          if(iflag_t_evo_4_vect_p .gt. 0) then
            write(label,'(a)') 'SGS_uxB'
            call sel_coord_vector_comp_labels(icoord_SGS_model_coef,    &
     &          label, lab(1) )
            call write_vector_label(file_id, lab(1))
!
          else
            write(label,'(a)') 'SGS_induction'
            call sel_coord_tensor_comp_labels(icoord_SGS_model_coef,    &
     &          label, lab(1) )
            call write_sym_tensor_label(file_id, lab(1))
          end if
!
        else if ( name_ak_sgs(i) .eq. fhd_SGS_buoyancy ) then
          call sel_coord_tensor_comp_labels(icoord_SGS_model_coef,      &
     &          fhd_SGS_buoyancy, lab(1) )
          call write_sym_tensor_label(file_id, lab(1))
!
        else if ( name_ak_sgs(i) .eq. fhd_SGS_comp_buo ) then
          call sel_coord_tensor_comp_labels(icoord_SGS_model_coef,      &
     &          fhd_SGS_comp_buo, lab(1) )
          call write_sym_tensor_label(file_id, lab(1))
!
        else if ( name_ak_sgs(i) .eq. fhd_SGS_c_flux ) then
          call sel_coord_vector_comp_labels(icoord_SGS_model_coef,      &
     &        fhd_SGS_c_flux, lab(1) )
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
      subroutine write_diff_comps_head(file_id)
!
      use m_control_parameter
      use m_ele_info_4_dynamical
      use m_phys_labels
      use m_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: file_id
      integer ( kind=kint) :: i
!
!
      do i = 1, num_diff_kinds
        if ( name_ak_diff(i) .eq. fhd_velo ) then
          write(file_id,'(a)') 'dVx_dx, dVx_dy, dVx_dz, '
          write(file_id,'(a)') 'dVy_dx, dVy_dy, dVy_dz, '
          write(file_id,'(a)') 'dVz_dx, dVz_dy, dVz_dz, '
        else if ( name_ak_diff(i) .eq. fhd_temp ) then
          write(file_id,'(a)') 'dT_dx, dT_dy, dT_dz, '
        else if ( name_ak_diff(i) .eq. fhd_magne ) then
          write(file_id,'(a)') 'dBx_dx, dBx_dy, dBx_dz, '
          write(file_id,'(a)') 'dBy_dx, dBy_dy, dBy_dz, '
          write(file_id,'(a)') 'dBz_dx, dBz_dy, dBz_dz, '
        else if ( name_ak_diff(i) .eq. fhd_light ) then
          write(file_id,'(a)')                                          &
     &         'composition_x, composition_y, composition_z, '
        else if ( name_ak_diff(i) .eq. fhd_SGS_h_flux ) then
          write(file_id,'(a)') 'SGS_hf_x, SGS_hf_y, SGS_hf_z, '
        else if ( name_ak_diff(i) .eq. fhd_SGS_m_flux ) then
          write(file_id,'(a)') 'SGS_mf_xx, SGS_mf_xy, SGS_mf_xz, '
          write(file_id,'(a)') 'SGS_mf_yx, SGS_mf_yy, SGS_mf_yz, '
          write(file_id,'(a)') 'SGS_mf_zx, SGS_mf_yz, SGS_mf_zz, '
        else if ( name_ak_diff(i) .eq. fhd_SGS_c_flux ) then
          write(file_id,'(a)') 'SGS_cf_x, SGS_cf_y, SGS_cf_z, '
        else if ( name_ak_sgs(i) .eq. fhd_SGS_maxwell_t ) then
          write(file_id,'(a)')                                          &
     &        'SGS_maxwell_xx, SGS_maxwell_xy, SGS_maxwell_xz, '
          write(file_id,'(a)')                                          &
     &        'SGS_maxwell_yx, SGS_maxwell_yy, SGS_maxwell_yz, '
          write(file_id,'(a)')                                          &
     &        'SGS_maxwell_zx, SGS_maxwell_yz, SGS_maxwell_zz, '
        else if ( name_ak_sgs(i) .eq. fhd_SGS_Lorentz) then
          write(file_id,'(a)') 'SGS_lor_xx, SGS_lor_xy, SGS_lor_xz, '
          write(file_id,'(a)') 'SGS_lor_yx, SGS_lor_yy, SGS_lor_yz, '
          write(file_id,'(a)') 'SGS_lor_zx, SGS_lor_yz, SGS_lor_zz, '
        else if ( name_ak_sgs(i) .eq. fhd_SGS_induction ) then
          if(iflag_t_evo_4_vect_p .gt. 0) then
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
