!element_grouping_udt.f90
!
      program element_grouping_udt
!
      use m_precision
!
      use m_constants
      use m_merdional_grouping_patch
      use m_ctl_params_ele_grp_udt
      use m_tave_SGS_model_coefs
      use m_field_file_format
!
      use t_ctl_data_ele_grp_udt
      use t_group_data
      use t_time_data
      use t_ucd_data
!
      use ucd_IO_select
      use read_layer_evo_file_header
      use set_model_coef_to_med_patch
!
      implicit none
!
!
      type(ctl_ele_grp_udt), save :: egrp_udt_ctl1
      type(group_data), save :: e_grp
      type(time_data), save :: med_time_IO
      type(ucd_data), save  :: ucd_med
!
      integer(kind = kint) :: istep_read, nd, ierr, icou, i, istart_grp
      integer(kind = kint), allocatable :: ncomp(:)
      real(kind = kreal)  :: time
!
!
      call read_control_ele_grp_udt(egrp_udt_ctl1)
      call set_control_ele_grp_udt(egrp_udt_ctl1)
!
      call read_med_grouping_patch(layerd_mesh_head, e_grp,             &
     &   start_ele_grp_name, istart_grp)
!
      write(*,*) 'fname_input: ', trim(group_data_file_name)
      call count_num_comp_layer_evo_file(id_org_file,                   &
     &    group_data_file_name, ithree, num_comp, num_layer)
!
!
      call allocate_model_coef_name
      call allocate_model_coef_array
!
      call read_field_name_evo_file(id_org_file,                        &
     &    group_data_file_name, ithree, num_comp, comp_name)
!
      write(*,*) 'num. of component and layer: ', num_comp, num_layer
      do nd = 1, num_comp
        write(*,'(i3,a2,a)') nd, ': ',  trim(comp_name(nd))
      end do
!
!    output grid data
!
!      tave_grp_ucd_param%iflag_format = iflag_udt
!      call sel_write_grd_file(-1, tave_grp_ucd_param, psf_ucd)
!      sdev_grp_ucd_param%iflag_format = iflag_udt
!      call sel_write_grd_file(-1, sdev_grp_ucd_param, psf_ucd)
!
!      call deallocate_med_grouping_patch
!
!
!
      write(*,'(a,39X)', advance='NO') 'read data for average:'
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
            rms_coef(1:num_layer,nd) = rms_coef(1:num_layer,nd)         &
     &                               + coef(1:num_layer,nd)**2
          end do
          icou = icou + 1
        end if
!        write(*,'(i15,a8,i15)') istep_read, ' count: ', icou
        write(*,'(38a1,i15,a8,i15)', advance='NO')                      &
     &           (char(8),i=1,38), istep_read, ' count: ', icou
        if(istep_read .ge. istep_end) exit
      end do
      close(id_org_file)
      write(*,*)
!
      do nd = 1, num_comp
        sigma_coef(1:num_layer,nd) = rms_coef(1:num_layer,nd)           &
     &                              - ave_coef(1:num_layer,nd)**2
!
        ave_coef(1:num_layer,nd)                                        &
     &           = ave_coef(1:num_layer,nd) / dble(icou)
        rms_coef(1:num_layer,nd)                                        &
     &           = sqrt(rms_coef(1:num_layer,nd) / dble(icou))
        sigma_coef(1:num_layer,nd)                                      &
     &           = sqrt(sigma_coef(1:num_layer,nd) / dble(icou))
      end do
!
!   Evaluate standard deviation
      call write_t_ave_m_coef_file(istep_read, time)
      call write_sigma_m_coef_file(istep_read, time)
!
      allocate(ncomp(num_comp))
      ncomp = 1
      call set_elemental_field_2_ucd(layerd_mesh_head, istart_grp,      &
     &    num_layer, num_comp, num_comp, ncomp, comp_name, ave_coef,    &
     &    ucd_med)
!
      tave_grp_ucd_param%iflag_format = iflag_vtk
      call sel_write_udt_file                                           &
     &   (-1, istep_read, tave_grp_ucd_param, med_time_IO, ucd_med)
      stop
!
      end program element_grouping_udt
