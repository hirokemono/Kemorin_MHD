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
!
      use t_group_data
      use t_time_data
      use t_ucd_data
      use t_psf_results
!
      use read_layer_evo_file_header
      use set_model_coef_to_med_patch
      use set_ucd_data_to_type
      use ucd_IO_select
      use cal_psf_rms_aves
!
      implicit none
!
!
      type(group_data), save :: e_grp
      type(time_data), save :: psf_time_IO
      type(ucd_data), save :: psf_ucd
      type(psf_results), save :: psf_data
!
      integer(kind = kint) :: istep_read, nd, ierr, istart_grp
      real(kind = kreal)  :: time
!
!
      call read_control_ele_grp_udt
      call set_control_ele_grp_udt
!
      call read_med_grouping_patch(layerd_mesh_head, e_grp,             &
     &    start_ele_grp_name, istart_grp)
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
      call set_ele_grp_patch_2_psf_grd(e_grp,                           &
     &    psf_data%psf_nod, psf_data%psf_ele, psf_data%psf_phys)
      call link_node_data_2_ucd(psf_data%psf_nod, psf_ucd)
      call link_ele_data_2_ucd(psf_data%psf_ele, psf_ucd)
!
      grp_ucd_param%iflag_format = iflag_udt
      call sel_write_grd_file(iminus, grp_ucd_param, psf_ucd)
      call deallocate_med_grouping_patch
!
!    output udt data
!
      call sqrt_of_rms_coefs(num_layer, num_comp, coef)
!
      tave_grp_ucd_param%iflag_format = iflag_udt
      call sel_write_udt_file(iminus, istep_start,                     &
     &    tave_grp_ucd_param, psf_time_IO, psf_ucd)
!
      do
        call read_evolution_data(id_org_file, num_comp, num_layer,      &
     &       istep_read, time, coef, ierr)
        if(ierr .gt. 0) exit
        write(*,*) 'read finish for step', istep_read, ierr
!
        if(mod((istep_read-istep_start),istep_inc) .eq. izero           &
     &     .and. istep_read.ge.istep_start) then
          call sqrt_of_rms_coefs(num_layer, num_comp, coef)
          call sel_write_udt_file                                       &
     &       (iminus, istep_start, grp_ucd_param, psf_time_IO, psf_ucd)
        end if
!
        if(istep_read .ge. istep_end) exit
      end do
      close(id_org_file)
!
      stop
      end program element_grouping_udt
