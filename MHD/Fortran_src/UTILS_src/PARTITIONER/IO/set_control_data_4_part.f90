!
!      module set_control_data_4_part
!
!      Written by Kemorin on Sep. 2007
!
!!      subroutine s_set_control_data_4_part                            &
!!     &         (part_ctl, comm_part, part_p)
!!      subroutine set_control_4_extend_sleeve                          &
!!     &         (id_rank, part_ctl, comm_part, part_p)
!!        type(control_data_4_partitioner), intent(in) :: part_ctl
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!!        type(ctl_param_partitioner), intent(inout) :: part_p
!
      module set_control_data_4_part
!
      use m_precision
      use m_constants
!
      use t_ctl_param_partitioner
!
      implicit none
!
      private :: set_FEM_mesh_ctl_4_part, set_partition_method
      private :: set_control_XYZ_RCB, set_control_SPH_RCB
      private :: set_control_EQ_XYZ, set_control_EQ_SPH
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_data_4_part                              &
     &         (part_ctl, comm_part, part_p)
!
      use t_control_data_4_part
      use t_partitioner_comm_table
      use m_default_file_prefix
!
      use m_file_format_switch
      use itp_table_IO_select_4_zlib
      use set_control_platform_data
!
      type(control_data_4_partitioner), intent(in) :: part_ctl
!
      type(partitioner_comm_tables), intent(inout) :: comm_part
      type(ctl_param_partitioner), intent(inout) :: part_p
!
      integer(kind = kint) :: i
!
!
      call turn_off_debug_flag_by_ctl(izero, part_ctl%part_plt)
      call set_control_mesh_def                                         &
     &   (part_ctl%part_plt, part_p%distribute_mesh_file)
!
!   set local data format
!
!
      if (part_ctl%single_plt%mesh_file_prefix%iflag .gt. 0) then
        part_p%global_mesh_file%file_prefix                             &
     &      = part_ctl%single_plt%mesh_file_prefix%charavalue
      else
        write(*,*) 'Set original mesh data'
        stop
      end if
      part_p%global_mesh_file%iflag_format                              &
     &     = choose_file_format(part_ctl%single_plt%mesh_file_fmt_ctl)
!
      part_p%nele_grp_ordering = 0
      if (part_ctl%ele_grp_ordering_ctl%icou .eq. 1) then
        part_p%nele_grp_ordering = part_ctl%ele_grp_ordering_ctl%num
!
        call alloc_ele_grp_ordering(part_p)
        do i = 1, part_p%nele_grp_ordering
          part_p%ele_grp_ordering(i)                                    &
     &       = part_ctl%ele_grp_ordering_ctl%c_tbl(i)
        end do
      end if
!
      call set_partition_method(part_ctl%part_method_ctl,               &
     &    part_ctl%new_part_method_ctl, part_ctl%selective_ghost_ctl,   &
     &    part_p)
!
      call set_FEM_mesh_ctl_4_part(part_ctl%part_Fmesh,                 &
     &    part_ctl%sleeve_level_old, part_ctl%element_overlap_ctl,      &
     &    comm_part, part_p)
!
      write(*,*) 'iflag_memory_conserve',                               &
     &          comm_part%iflag_memory_conserve
      write(*,*) 'iflag_viewer_output', part_p%iflag_viewer_output
      if(part_p%n_overlap .eq. 1) then
        write(*,*) 'element overlapping flag: ',                        &
     &            part_p%iflag_sleeve_ele
      end if
!
!
      if(part_p%NTYP_div .eq. iPART_RCB_XYZ) then
        call set_control_XYZ_RCB(part_ctl%RCB_dir_ctl, part_p)
      else if(part_p%NTYP_div .eq. iPART_RCB_SPH) then
        call set_control_SPH_RCB(part_ctl%RCB_dir_ctl, part_p)
!
      else if (part_p%NTYP_div .eq. iPART_EQ_XYZ                        &
     &    .or. part_p%NTYP_div .eq. iPART_EQV_XYZ) then
        call set_control_EQ_XYZ(part_ctl%ndomain_section_ctl, part_p)
      else if (part_p%NTYP_div.eq.iPART_CUBED_SPHERE                    &
     &    .or. part_p%NTYP_div.eq.iPART_EQ_SPH                          &
     &    .or. part_p%NTYP_div.eq.iPART_LAYER_SPH) then
        call set_control_EQ_SPH(part_ctl%ndomain_section_ctl,           &
     &    part_ctl%ele_grp_layering_ctl, part_ctl%sphere_file_name_ctl, &
     &    part_p)
!
      else if (part_p%NTYP_div .eq. iPART_DECMP_MESH_TBL                &
     &    .or. part_p%NTYP_div .eq. iPART_FINE_MESH_TBL) then
        if (part_ctl%domain_group_file_ctl%iflag .eq. 1) then
          part_p%fname_subdomain                                        &
     &        = part_ctl%domain_group_file_ctl%charavalue
      else
        write(*,*) 'set domain table file name'
        stop
      end if
!
      if(part_p%NTYP_div .eq. iPART_FINE_MESH_TBL) then
        if (part_ctl%itp_tbl_head_ctl%iflag .eq. 1) then
          part_p%finer_inter_file_head                                  &
     &          = part_ctl%itp_tbl_head_ctl%charavalue
        else
          write(*,*) 'set interpolate file name'
          stop
        end if
!
        call set_file_control_params(def_finer_mesh,                    &
     &      part_ctl%finer_mesh_head_ctl, part_ctl%finer_mesh_fmt_ctl,  &
     &      part_p%finer_mesh_file)
!
        ifmt_itp_table_file                                             &
     &       = choose_file_format(part_ctl%itp_tbl_format_ctl)
      end if
!
      else if(part_p%NTYP_div .eq. iPART_MeTiS_RSB) then
        if (part_ctl%metis_domain_file_ctl%iflag .eq. 1) then
          part_p%metis_sdom_name                                        &
     &           = part_ctl%metis_domain_file_ctl%charavalue
        else
          write(*,*) 'set MeTiS input file name'
          stop
        end if
!
      else if (iPART_GEN_MeTiS .eq. iPART_GEN_MeTiS) then
        if (part_ctl%metis_input_file_ctl%iflag .eq. 1) then
          part_p%metis_file_name                                        &
     &           = part_ctl%metis_input_file_ctl%charavalue
        else
          write(*,*) 'set MeTiS domain file name'
          stop
        end if
!
        write(*,'(/,"generate MeTiS/RSB input")')
        write(*,'(/,"*** MeTiS/RSB input  ", a)')                       &
     &       part_p%metis_file_name
      end if
!
      end subroutine s_set_control_data_4_part
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_extend_sleeve                            &
     &         (id_rank, part_ctl, comm_part, part_p)
!
      use t_control_data_4_part
      use t_partitioner_comm_table
      use m_default_file_prefix
!
      use m_file_format_switch
      use itp_table_IO_select_4_zlib
      use set_control_platform_data
!
      integer, intent(in) :: id_rank
      type(control_data_4_partitioner), intent(in) :: part_ctl
!
      type(partitioner_comm_tables), intent(inout) :: comm_part
      type(ctl_param_partitioner), intent(inout) :: part_p
!
!
      call turn_off_debug_flag_by_ctl(id_rank, part_ctl%part_plt)
      call set_control_mesh_def                                         &
     &   (part_ctl%part_plt, part_p%distribute_mesh_file)
!
!   set local data format
!
!
      if (part_ctl%single_plt%mesh_file_prefix%iflag .gt. 0) then
        part_p%global_mesh_file%file_prefix                             &
     &      = part_ctl%single_plt%mesh_file_prefix%charavalue
      else
        write(*,*) 'Set original mesh data'
        stop
      end if
      part_p%global_mesh_file%iflag_format                              &
     &   = choose_file_format(part_ctl%single_plt%mesh_file_fmt_ctl)
!
      call set_FEM_mesh_ctl_4_part(part_ctl%part_Fmesh,                 &
     &    part_ctl%sleeve_level_old, part_ctl%element_overlap_ctl,      &
     &    comm_part, part_p)
!
      if(id_rank .ne. 0) return
      write(*,*) 'sleeve level :', part_p%n_overlap
      write(*,*) 'iflag_memory_conserve',                               &
     &          comm_part%iflag_memory_conserve
      write(*,*) 'iflag_viewer_output', part_p%iflag_viewer_output
      if(part_p%n_overlap .eq. 1) then
        write(*,*) 'element overlapping flag: ',                        &
     &            part_p%iflag_sleeve_ele
      end if
!
      end subroutine set_control_4_extend_sleeve
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_FEM_mesh_ctl_4_part                                &
     &         (part_Fmesh, sleeve_level_old, element_overlap_ctl,      &
     &          comm_part, part_p)
!
      use t_ctl_data_4_FEM_mesh
      use t_control_elements
      use t_partitioner_comm_table
      use m_default_file_prefix
!
      use m_file_format_switch
      use skip_comment_f
!
      type(FEM_mesh_control), intent(in) :: part_Fmesh
      type(read_integer_item), intent(in) :: sleeve_level_old
      type(read_character_item), intent(in)  :: element_overlap_ctl
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
!
      comm_part%iflag_memory_conserve = 0
      if(part_Fmesh%memory_conservation_ctl%iflag .gt. 0                &
     &  .and. yes_flag(part_Fmesh%memory_conservation_ctl%charavalue)   &
     &   ) then
        comm_part%iflag_memory_conserve = 1
      end if
!
      part_p%iflag_viewer_output = 0
      if(part_Fmesh%FEM_viewer_output_switch%iflag .gt. 0               &
     &  .and. yes_flag(part_Fmesh%FEM_viewer_output_switch%charavalue)  &
     &   ) then
        part_p%iflag_viewer_output = 1
      end if
!
      if(part_Fmesh%FEM_sleeve_level_ctl%iflag .gt. 0) then
        part_p%n_overlap = part_Fmesh%FEM_sleeve_level_ctl%intvalue
      else if(sleeve_level_old%iflag .gt. 0) then
        part_p%n_overlap = sleeve_level_old%intvalue
      else
        part_p%n_overlap = 1
      end if
      if(part_p%n_overlap .lt. 1) part_p%n_overlap = 1
!
      part_p%iflag_sleeve_ele = 0
      if(part_p%n_overlap .eq. 1) then
        if(part_Fmesh%FEM_element_overlap_ctl%iflag .gt. 0) then
          part_p%iflag_sleeve_ele = 1
          part_p%n_overlap =    2
        else if(element_overlap_ctl%iflag .gt. 0) then
          part_p%iflag_sleeve_ele = 1
          part_p%n_overlap =    2
        end if
      end if
!
      end subroutine set_FEM_mesh_ctl_4_part
!
! -----------------------------------------------------------------------
!
      subroutine set_partition_method(part_method_ctl,                  &
     &          new_part_method_ctl, selective_ghost_ctl, part_p)
!
      use t_control_elements
      use skip_comment_f
!
      type(read_character_item), intent(in) :: part_method_ctl
      type(read_character_item), intent(in) :: new_part_method_ctl
      type(read_character_item), intent(in) :: selective_ghost_ctl
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
!
      part_p%iflag_LIC_partition = 0
      if(new_part_method_ctl%iflag .gt. 0) then
        if( cmp_no_case(new_part_method_ctl%charavalue,'YES')) then
          part_p%iflag_LIC_partition = 1
        end if
      end if
      write(*,*) 'ifag_LIC_partition', part_p%iflag_LIC_partition

      part_p%iflag_new_ghost_cell = 0
      if(selective_ghost_ctl%iflag .gt. 0) then
        if( cmp_no_case(selective_ghost_ctl%charavalue,'YES')) then
          part_p%iflag_new_ghost_cell = 1
        end if
      end if
      write(*,*) 'iflag_new_ghost_cell', part_p%iflag_new_ghost_cell
!
      if (part_method_ctl%iflag .gt. 0) then
        if(     cmp_no_case(part_method_ctl%charavalue,'RCB_xyz')       &
     &     .or. cmp_no_case(part_method_ctl%charavalue,'RCB')) then
            part_p%NTYP_div = iPART_RCB_XYZ
!
        else if(cmp_no_case(part_method_ctl%charavalue,'RCB_sph')) then
            part_p%NTYP_div = iPART_RCB_SPH
!
        else if(cmp_no_case(part_method_ctl%charavalue,'ES')            &
     &     .or. cmp_no_case(part_method_ctl%charavalue,'ES_xyz')) then
            part_p%NTYP_div = iPART_EQ_XYZ
!
        else if(cmp_no_case(part_method_ctl%charavalue,'ES_vol')) then
            part_p%NTYP_div = iPART_EQV_XYZ
!
        else if(cmp_no_case(part_method_ctl%charavalue,'ES_sph')) then
            part_p%NTYP_div = iPART_EQ_SPH
!
        else if(cmp_no_case(part_method_ctl%charavalue,                 &
     &                      'ES_layered_sph')) then
            part_p%NTYP_div = iPART_LAYER_SPH
!
        else if( cmp_no_case(part_method_ctl%charavalue,                &
     &                       'MeTiS_input')) then
            part_p%NTYP_div = iPART_GEN_MeTiS
!
        else if( cmp_no_case(part_method_ctl%charavalue,                &
     &                       'MeTiS_RSB')) then
            part_p%NTYP_div = iPART_MeTiS_RSB
!
        else if( cmp_no_case(part_method_ctl%charavalue, 'Cubed_sph')   &
     &      .or. cmp_no_case(part_method_ctl%charavalue,                &
     &                      'Cubed_sphere')) then
            part_p%NTYP_div = iPART_CUBED_SPHERE
!
        else if( cmp_no_case(part_method_ctl%charavalue, 'finer_mesh')  &
     &      .or. cmp_no_case(part_method_ctl%charavalue,                &
     &                       'Divide_by_finer_mesh') ) then
            part_p%NTYP_div = iPART_FINE_MESH_TBL
!
        else if( cmp_no_case(part_method_ctl%charavalue, 'Decomp_data') &
     &      .or. cmp_no_case(part_method_ctl%charavalue,                &
     &                       'decomposit_data')) then
            part_p%NTYP_div = iPART_FINE_MESH_TBL
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
        write(*,*) 'decomposition code:', part_p%NTYP_div
!
      else
        write(*,*) 'Set partitioning method'
        stop
      end if
!
      end subroutine set_partition_method
!
! -----------------------------------------------------------------------
!
      subroutine set_control_XYZ_RCB(RCB_dir_ctl, part_p)
!
      use t_read_control_arrays
      use skip_comment_f
!
      type(ctl_array_chara), intent(in) :: RCB_dir_ctl
      type(ctl_param_partitioner), intent(inout) :: part_p
!
      integer(kind = kint) :: i, icou
!
!
        part_p%NPOWER_rcb = RCB_dir_ctl%num
        part_p%num_domain = 2**part_p%NPOWER_rcb
        call alloc_rcb_directions(part_p)
!
        icou = 0
        do i = 1, RCB_dir_ctl%num
          if     (cmp_no_case(RCB_dir_ctl%c_tbl(i), 'X')) then
            icou = icou + 1
            part_p%idir_rcb(icou) = 1
          else if(cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Y')) then
            icou = icou + 1
            part_p%idir_rcb(icou) = 2
          else if(cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Z')) then
            icou = icou + 1
            part_p%idir_rcb(icou) = 3
          end if
        end do
        if (icou .lt. part_p%NPOWER_rcb) then
          write(*,*) 'Set correct direction'
          stop
        end if
!
      end subroutine set_control_XYZ_RCB
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SPH_RCB(RCB_dir_ctl, part_p)
!
      use t_read_control_arrays
      use skip_comment_f
!
      type(ctl_array_chara), intent(in) :: RCB_dir_ctl
      type(ctl_param_partitioner), intent(inout) :: part_p
!
      integer(kind = kint) :: i, icou
!
!
        part_p%NPOWER_rcb = RCB_dir_ctl%num
        part_p%num_domain = 2**part_p%NPOWER_rcb
        call alloc_rcb_directions(part_p)
!
        icou = 0
        do i = 1, RCB_dir_ctl%num
          if(      cmp_no_case(RCB_dir_ctl%c_tbl(i), 'R')               &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Radius')          &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Radial')) then
            icou = icou + 1
            part_p%idir_rcb(icou) = 1
!
          else if( cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Meridional')      &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Theta')           &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Elevation')) then
            icou = icou + 1
            part_p%idir_rcb(icou) = 2
!
          else if( cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Longitudinal')    &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Phi')             &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Azimuth')) then
            icou = icou + 1
            part_p%idir_rcb(icou) = 3
          end if
        end do
        if (icou .lt. part_p%NPOWER_rcb) then
          write(*,*) 'Set correct direction'
          stop
        end if
!
        write(*,'(/,"### Spherical RECURSIVE COORDINATE BiSECTION")')
        write (*,*)  "number of level: ", part_p%NPOWER_rcb
        write (*,'(" Direction Code r:1, theta:2, phi:3")')
        write  (*,*) part_p%idir_rcb(1:part_p%NPOWER_rcb)
!
      end subroutine set_control_SPH_RCB
!
! -----------------------------------------------------------------------
!
      subroutine set_control_EQ_XYZ(ndomain_section_ctl, part_p)
!
      use t_read_control_arrays
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: ndomain_section_ctl
      type(ctl_param_partitioner), intent(inout) :: part_p
!
      integer(kind = kint) :: i
!
!
      if(ndomain_section_ctl%num .ne. 3)                                &
     &         stop 'number of subdomain should be 3 directions'
!
      part_p%ndivide_eb(1:3) = 1
      do i = 1, ndomain_section_ctl%num
        if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'X')               &
     &        ) part_p%ndivide_eb(1) = ndomain_section_ctl%ivec(i)
        if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Y')               &
     &        ) part_p%ndivide_eb(2) = ndomain_section_ctl%ivec(i)
        if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Z')               &
     &        ) part_p%ndivide_eb(3) = ndomain_section_ctl%ivec(i)
      end do
      part_p%num_domain = part_p%ndivide_eb(1) * part_p%ndivide_eb(2)   &
     &                   * part_p%ndivide_eb(3)
!
      end subroutine set_control_EQ_XYZ
!
! -----------------------------------------------------------------------
!
      subroutine set_control_EQ_SPH(ndomain_section_ctl,                &
     &          ele_grp_layering_ctl, sphere_file_name_ctl, part_p)
!
      use t_read_control_arrays
      use t_control_elements
      use skip_comment_f
!
      type(ctl_array_ci), intent(in) :: ndomain_section_ctl
      type(ctl_array_chara), intent(in) :: ele_grp_layering_ctl
      type(read_character_item), intent(in) :: sphere_file_name_ctl
!
      type(ctl_param_partitioner), intent(inout) :: part_p
!
      integer(kind = kint) :: i
!
!
        if(ndomain_section_ctl%num .ne. 3)                              &
     &         stop 'number of subdomain should be 3 directions'
!
        part_p%ndivide_eb(1:3) = 1
        do i = 1, ndomain_section_ctl%num
          if(      cmp_no_case(ndomain_section_ctl%c_tbl(i), 'R')       &
     &        .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Radius')  &
     &        .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Radial')  &
     &       ) then
            part_p%ndivide_eb(1) = ndomain_section_ctl%ivec(i)
!
          else if( cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Theta')   &
     &     .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Meridional') &
     &     .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Elevation')  &
     &        ) then
            part_p%ndivide_eb(2) = ndomain_section_ctl%ivec(i)
!
          else if( cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Phi')     &
     &    .or. cmp_no_case(ndomain_section_ctl%c_tbl(i),'Longitudinal') &
     &    .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Azimuth')     &
     &        ) then
            part_p%ndivide_eb(3) = ndomain_section_ctl%ivec(i)
          end if
        end do
        part_p%num_domain = part_p%ndivide_eb(1) * part_p%ndivide_eb(2) &
     &                     * part_p%ndivide_eb(3)
!
        if(part_p%NTYP_div .eq. iPART_LAYER_SPH) then
          part_p%num_egrp_layer = ele_grp_layering_ctl%num
          call alloc_ele_grp_layer_name(part_p)
!
          do i = 1, part_p%num_egrp_layer
            part_p%grp_layer_name(i) = ele_grp_layering_ctl%c_tbl(i)
          end do
!
        else if(part_p%NTYP_div .eq. iPART_CUBED_SPHERE) then
          part_p%iflag_sphere_data = sphere_file_name_ctl%iflag
          if(part_p%iflag_sphere_data .eq. 1) then
            part_p%sphere_data_file_name                                &
     &                = sphere_file_name_ctl%charavalue
            write(*,*) 'Sphere surface correction file: ',              &
     &              trim(part_p%sphere_data_file_name)
          else
            write(*,*) 'No Sphere surface correction file '
          end if
        end if
!
      end subroutine set_control_EQ_SPH
!
! -----------------------------------------------------------------------
!
      end module set_control_data_4_part
