!
!      module set_control_data_4_part
!
!      Written by Kemorin on Sep. 2007
!
!      subroutine s_set_control_data_4_part
!
      module set_control_data_4_part
!
      use m_precision
!
      implicit    none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_data_4_part
!
      use m_control_data_4_part
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
!
      use m_ctl_param_partitioner
      use m_partitioner_comm_table
      use read_sphere_surface
      use m_subdomain_table_IO
      use m_metis_IO
      use m_file_format_switch
      use itp_table_IO_select_4_zlib
      use set_control_platform_data
      use set_ctl_params_2nd_files
      use skip_comment_f
!
      integer(kind = kint) :: i
!
!
      call set_control_mesh_def(plt1, distribute_mesh_file)
!
!   set local data format
!
!
      iflag_memory_conserve = 1
      if(plt1%memory_conservation_ctl%iflag .gt. 0                      &
     &  .and. no_flag(plt1%memory_conservation_ctl%charavalue)) then
        iflag_memory_conserve = 0
      end if
!
      write(*,*) 'iflag_memory_conserve', iflag_memory_conserve
!
!
      if (org_mesh_head_ctl%iflag .gt. 0) then
        global_mesh_file%file_prefix = org_mesh_head_ctl%charavalue
      else
        write(*,*) 'Set original mesh data'
        stop
      end if
      call choose_file_format                                           &
     &   (org_mesh_file_fmt_ctl, global_mesh_file%iflag_format)
!
      write(*,*) 'i_part_method', part_method_ctl%iflag
!
      nele_grp_ordering = 0
      if (ele_grp_ordering_ctl%icou .eq. 1) then
        nele_grp_ordering = ele_grp_ordering_ctl%num
!
        allocate(ele_grp_ordering(nele_grp_ordering))
        allocate(igrp_ele_ordering(nele_grp_ordering))
        ele_grp_ordering(1:nele_grp_ordering)                           &
     &      = ele_grp_ordering_ctl%c_tbl(1:nele_grp_ordering)
!
        call dealloc_ele_grp_ordering_ctl
      end if
!
      if (part_method_ctl%iflag .gt. 0) then
        if(     cmp_no_case(part_method_ctl%charavalue,'RCB_xyz')       &
     &     .or. cmp_no_case(part_method_ctl%charavalue,'RCB')) then
            NTYP_div = iPART_RCB_XYZ
!
        else if(cmp_no_case(part_method_ctl%charavalue,'RCB_sph')) then
            NTYP_div = iPART_RCB_SPH
!
        else if(cmp_no_case(part_method_ctl%charavalue,'ES')            &
     &     .or. cmp_no_case(part_method_ctl%charavalue,'ES_xyz')) then
            NTYP_div = iPART_EQ_XYZ
!
        else if(cmp_no_case(part_method_ctl%charavalue,'ES_sph')) then
            NTYP_div = iPART_EQ_SPH
!
        else if(cmp_no_case(part_method_ctl%charavalue,                 &
     &                      'ES_layered_sph')) then
            NTYP_div = iPART_LAYER_SPH
!
        else if( cmp_no_case(part_method_ctl%charavalue,                &
     &                       'MeTiS_input')) then
            NTYP_div = iPART_GEN_MeTiS
!
        else if( cmp_no_case(part_method_ctl%charavalue,                &
     &                       'MeTiS_RSB')) then
            NTYP_div = iPART_MeTiS_RSB
!
        else if( cmp_no_case(part_method_ctl%charavalue, 'Cubed_sph')   &
     &      .or. cmp_no_case(part_method_ctl%charavalue,                &
     &                      'Cubed_sphere')) then
            NTYP_div = iPART_CUBED_SPHERE
!
        else if( cmp_no_case(part_method_ctl%charavalue, 'finer_mesh')  &
     &      .or. cmp_no_case(part_method_ctl%charavalue,                &
     &                       'Divide_by_finer_mesh') ) then
            NTYP_div = iPART_FINE_MESH_TBL
!
        else if( cmp_no_case(part_method_ctl%charavalue, 'Decomp_data') &
     &      .or. cmp_no_case(part_method_ctl%charavalue,                &
     &                       'decomposit_data')) then
            NTYP_div = iPART_FINE_MESH_TBL
        end if
!
        write (*,'(/," *********************************")')
        write (*,'(  " ||                             ||")')
        write (*,'(  " || GeoFEM Partitioner Ver.x.00 ||")')
        write (*,'(  " ||                  by Kemorin ||")')
        write (*,'(  " ||                             ||")')
        write (*,'(  " *********************************")')
        write (*,*) ' '
        write (*,'(/,"# PARTITIONING METHOD")')
        write (*,'(  "  RCB                                (1)")')
        write (*,'(  "  RCB(spherical corrdinate)          (2)")')
        write (*,'(  "  MeTiS/RSB                          (3)")')
        write (*,'(  "  generate MeTiS/RSB INPUT           (4)")')
        write (*,'(  "  Divide on sphere[cube base]        (5)")')
        write (*,'(  "  Divide by refereing finer grid     (6)")')
        write (*,'(  "  Divide using decomposition data    (7)")')
        write (*,'(  "  Divide equality with xyz direction (8)")')
        write (*,'(  "  Divide equality with rtp direction (9)")')
        write(*,*) 'decomposition code:', NTYP_div
!
      else
        write(*,*) 'Set partitioning method'
        stop
      end if
!
!
      if (sleeve_level_ctl%iflag .gt. 0) then
        n_overlap = sleeve_level_ctl%intvalue
      else
        n_overlap = 1
      end if
      write(*,*) 'sleeve level :', n_overlap
!
      if (element_overlap_ctl%iflag .gt. 0 .and. n_overlap .eq. 1) then
        if(yes_flag(element_overlap_ctl%charavalue)) then
          i_sleeve_ele = 1
          n_overlap =    2
        else
          i_sleeve_ele = 0
        end if
        write(*,*) 'element overlapping code: ', i_sleeve_ele
      end if
!
!
      if (NTYP_div .eq. iPART_RCB_XYZ) then
        NPOWER_rcb = RCB_dir_ctl%num
        num_domain = 2**NPOWER_rcb
        allocate( idir_rcb(NPOWER_rcb) )
!
        NPOWER_rcb = 0
        do i = 1, RCB_dir_ctl%num
          if     (cmp_no_case(RCB_dir_ctl%c_tbl(i), 'X')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 1
          else if(cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Y')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 2
          else if(cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Z')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 3
          end if
        end do
        if (NPOWER_rcb .lt. RCB_dir_ctl%num) then
          write(*,*) 'Set correct direction'
          stop
        end if
!
        call dealloc_num_bisection_ctl
!
      else if( NTYP_div .eq. iPART_RCB_SPH) then
        NPOWER_rcb = RCB_dir_ctl%num
        num_domain = 2**NPOWER_rcb
        allocate( idir_rcb(NPOWER_rcb) )
!
        NPOWER_rcb = 0
        do i = 1, RCB_dir_ctl%num
          if(      cmp_no_case(RCB_dir_ctl%c_tbl(i), 'R')               &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Radius')          &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Radial')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 1
!
          else if( cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Meridional')      &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Theta')           &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Elevation')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 2
!
          else if( cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Longitudinal')    &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Phi')             &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Azimuth')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 3
          end if
        end do
        if (NPOWER_rcb .lt. RCB_dir_ctl%num) then
          write(*,*) 'Set correct direction'
          stop
        end if
!
        call dealloc_num_bisection_ctl
!
        write(*,'(/,"### Spherical RECURSIVE COORDINATE BiSECTION")')
        write (*,*)  "number of level: ", NPOWER_rcb
        write (*,'(" Direction Code r:1, theta:2, phi:3")')
        write  (*,*) idir_rcb(1:NPOWER_rcb)
!
!
      else if (NTYP_div .eq. iPART_EQ_XYZ) then
        if(ndomain_section_ctl%num .ne. 3)                              &
     &         stop 'number of subdomain should be 3 directions'
!
        ndivide_eb(1:3) = 1
        do i = 1, ndomain_section_ctl%num
          if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'X')             &
     &          ) ndivide_eb(1) = ndomain_section_ctl%ivec(i)
          if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Y')             &
     &          ) ndivide_eb(2) = ndomain_section_ctl%ivec(i)
          if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Z')             &
     &          ) ndivide_eb(3) = ndomain_section_ctl%ivec(i)
        end do
        num_domain = ndivide_eb(1)*ndivide_eb(2)*ndivide_eb(3)
!
        call dealloc_num_subdomains_ctl
!
      else if (NTYP_div.eq.iPART_CUBED_SPHERE                           &
     &    .or. NTYP_div.eq.iPART_EQ_SPH                                 &
     &    .or. NTYP_div.eq.iPART_LAYER_SPH) then
        if(ndomain_section_ctl%num .ne. 3)                              &
     &         stop 'number of subdomain should be 3 directions'
!
        ndivide_eb(1:3) = 1
        do i = 1, ndomain_section_ctl%num
          if(      cmp_no_case(ndomain_section_ctl%c_tbl(i), 'R')       &
     &        .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Radius')  &
     &        .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Radial')  &
     &       ) then
            ndivide_eb(1) = ndomain_section_ctl%ivec(i)
!
          else if( cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Theta')   &
     &     .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Meridional') &
     &     .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Elevation')  &
     &        ) then
            ndivide_eb(2) = ndomain_section_ctl%ivec(i)
!
          else if( cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Phi')     &
     &    .or. cmp_no_case(ndomain_section_ctl%c_tbl(i),'Longitudinal') &
     &    .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Azimuth')     &
     &        ) then
            ndivide_eb(3) = ndomain_section_ctl%ivec(i)
          end if
        end do
        num_domain = ndivide_eb(1)*ndivide_eb(2)*ndivide_eb(3)
!
        call dealloc_num_subdomains_ctl
!
        if(NTYP_div .eq. iPART_LAYER_SPH) then
          num_egrp_layer = ele_grp_layering_ctl%num
          allocate(grp_layer_name(num_egrp_layer))
!
          if (num_egrp_layer .gt. 0) then
            grp_layer_name(1:num_egrp_layer)                            &
     &          = ele_grp_layering_ctl%c_tbl(1:num_egrp_layer)
          end if
          call dealloc_ele_grp_layer_ctl
!
        else if ( NTYP_div .eq. iPART_CUBED_SPHERE) then
          iflag_sphere_data = sphere_file_name_ctl%iflag
          if (iflag_sphere_data .eq. 1) then
            sphere_data_file_name = sphere_file_name_ctl%charavalue
            write(*,*) 'Sphere surface correction file: ',              &
     &              trim(sphere_data_file_name)
          else
            write(*,*) 'No Sphere surface correction file '
          end if
        end if
!
!
      else if (NTYP_div .eq. iPART_DECMP_MESH_TBL                       &
     &    .or. NTYP_div .eq. iPART_FINE_MESH_TBL) then
        if (domain_group_file_ctl%iflag .eq. 1) then
          fname_subdomain = domain_group_file_ctl%charavalue
        else
          write(*,*) 'set domain table file name'
          stop
        end if
!
        if(NTYP_div .eq. iPART_FINE_MESH_TBL) then
          if (itp_tbl_head_ctl%iflag .eq. 1) then
            finer_inter_file_head = itp_tbl_head_ctl%charavalue
          else
            write(*,*) 'set interpolate file name'
            stop
          end if
!
          call set_file_control_params(def_finer_mesh,                  &
      &       finer_mesh_head_ctl, finer_mesh_fmt_ctl, finer_mesh_file)
!
          call choose_file_format                                       &
     &       (itp_tbl_format_ctl, ifmt_itp_table_file)
        end if
!
      else if(NTYP_div .eq. iPART_MeTiS_RSB) then
        if (metis_domain_file_ctl%iflag .eq. 1) then
          metis_sdom_name = metis_domain_file_ctl%charavalue
        else
          write(*,*) 'set MeTiS input file name'
          stop
        end if
!
      else if (iPART_GEN_MeTiS .eq. iPART_GEN_MeTiS) then
        if (metis_input_file_ctl%iflag .eq. 1) then
          metis_file_name = metis_input_file_ctl%charavalue
        else
          write(*,*) 'set MeTiS domain file name'
          stop
        end if
!
        write ( *,'(/,"generate MeTiS/RSB input")')
        write  ( *,'(/,"*** GRID  file   ", a)')  org_mesh_header
        write  ( *,'(/,"*** MeTiS/RSB input  ", a)')  metis_file_name
      end if
!
      end subroutine s_set_control_data_4_part
!
! -----------------------------------------------------------------------
!
      end module set_control_data_4_part
