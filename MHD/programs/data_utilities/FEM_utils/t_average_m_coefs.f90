!element_grouping_udt.f90
!
      program element_grouping_udt
!
      use m_precision
!
      use m_constants
      use m_merdional_grouping_patch
      use m_ctl_data_ele_grp_udt
      use m_ctl_params_ele_grp_udt
      use m_tave_SGS_model_coefs
      use m_field_file_format
      use m_psf_results
!
      use t_ucd_data
!
      use ucd_IO_select
      use read_layer_evo_file_header
      use set_model_coef_to_med_patch
!
      implicit none
!
!
      type(ucd_data), save :: psf_ucd
!
      integer(kind = kint) :: istep_read, nd, ierr, icou, i
      real(kind = kreal)  :: time
!
!
      call read_control_ele_grp_udt
      call set_control_ele_grp_udt
!
      call read_med_grouping_patch(layerd_mesh_head)
!
      write(*,*) 'fname_input: ', trim(group_data_file_name)
      call count_num_comp_layer_evo_file(id_org_file,                   &
     &      group_data_file_name, ithree, num_comp, num_layer)
!
!
      call allocate_model_coef_name
      call allocate_model_coef_array
!
      call read_field_name_evo_file(id_org_file,                        &
     &      group_data_file_name, ithree, num_comp, comp_name)
!
      write(*,*) 'num. of component and layer: ', num_comp, num_layer
      do nd = 1, num_comp
        write(*,'(i3,a2,a)') nd, ': ',  trim(comp_name(nd))
      end do
!
!    output grid data
!
      call set_ele_grp_patch_2_psf_grd
      call set_psf_mesh_to_ucd_mesh(psf_ucd)
!
      psf_ucd%ifmt_file = iflag_udt
      psf_ucd%file_prefix = tave_grp_udt_head
      call sel_write_grd_file(iminus, psf_ucd)
      psf_ucd%file_prefix = tsig_grp_udt_head
      call sel_write_grd_file(iminus, psf_ucd)
!
      call deallocate_med_grouping_patch
!
!
!
      write(*,'(a,29X)', advance='NO') 'read data for average:'
      icou = 0
      do
        call read_evolution_data(id_org_file, num_comp, num_layer,      &
    &       istep_read, time, coef, ierr)
        if(ierr .gt. 0) exit
!
        if(mod((istep_read-istep_start),istep_inc) .eq. izero           &
     &     .and. istep_read.ge.istep_start) then
          if(icou .eq. 0) start_time = time
          do nd = 1, num_comp
            ave_coef(1:num_layer,nd) = ave_coef(1:num_layer,nd)         &
     &                               + coef(1:num_layer,nd)
          end do
          icou = icou + 1
        end if
        write(*,'(28a1,i15,a8,i15)', advance='NO')                      &
     &           (char(8),i=1,28), istep_read, ' count: ', icou
        if(istep_read .ge. istep_end) exit
      end do
      close(id_org_file)
      write(*,*)
!
      do nd = 1, num_comp
        ave_coef(1:num_layer,nd) = ave_coef(1:num_layer,nd)/dble(icou)
      end do
!
!   Evaluate standard deviation
!
      call read_field_name_evo_file(id_org_file, group_data_file_name,  &
     &    ithree, num_comp, comp_name)
!
      write(*,'(a,29X)', advance='NO') 'read data for deviation:'
      icou = 0
      do
        call read_evolution_data(id_org_file, num_comp, num_layer,      &
    &       istep_read, time, coef, ierr)
        if(ierr .gt. 0) exit
!
        if(mod((istep_read-istep_start),istep_inc) .eq. izero           &
     &     .and. istep_read.ge.istep_start) then
          do nd = 1, num_comp
            sigma_coef(1:num_layer,nd) = sigma_coef(1:num_layer,nd)     &
     &         + (coef(1:num_layer,nd) - ave_coef(1:num_layer,nd) )**2
          end do
          icou = icou + 1
        end if
        write(*,'(28a1,i15,a8,i15)', advance='NO')                      &
     &           (char(8),i=1,28), istep_read, ' count: ', icou
!
        if(istep_read .ge. istep_end) exit
      end do
      close(id_org_file)
      write(*,*)
!
      do nd = 1, num_comp
        sigma_coef(1:num_layer,nd) = sigma_coef(1:num_layer,nd)         &
     &                             / dble(icou)
      end do
!
      call sqrt_of_rms_coefs(num_layer, num_comp, ave_coef)
      call sqrt_of_rms_coefs(num_layer, num_comp, sigma_coef)
!
      call write_t_ave_m_coef_file(istep_read, time)
      call write_sigma_m_coef_file(istep_read, time)
!
      call set_field_to_med_patch(num_layer, num_comp,                  &
     &    comp_name, ave_coef)
!
      call set_psf_mesh_to_ucd_field(psf_ucd)
!
      psf_ucd%ifmt_file = iflag_udt
      psf_ucd%file_prefix = tave_grp_udt_head
      call sel_write_udt_file(iminus, istep_read, psf_ucd)
!
      call set_field_to_med_patch(num_layer, num_comp,                  &
     &    comp_name, sigma_coef)
      call sel_write_udt_file(iminus, istep_read, psf_ucd)
!
      end program element_grouping_udt
