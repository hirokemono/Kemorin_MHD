!output_newdomain_ucd.f90
!      module output_newdomain_ucd
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine assemble_2nd_udt_phys(istep)
!      subroutine assemble_2nd_udt_nesh
!
      module output_newdomain_ucd
!
      use m_precision
!
      use m_constants
      use m_control_param_merge
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_ucd_data
!
      implicit none
!
      private :: copy_domain_data_from_global
      private :: copy_node_posi_from_global
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine assemble_2nd_udt_phys(istep)
!
      use ucd_IO_select
!
      integer (kind = kint), intent(in) :: istep
      integer (kind = kint) :: ip, my_rank
!
!
      num_field_ucd = merged_fld%num_phys
      ntot_comp_ucd = merged_fld%ntot_phys
      call allocate_ucd_phys_name
!
      istack_comp_ucd(0:num_field_ucd)                                  &
     &     = merged_fld%istack_component(0:num_field_ucd)
      num_comp_ucd(1:num_field_ucd)                                     &
     &     = merged_fld%num_component(1:num_field_ucd)
      phys_name_ucd(1:num_field_ucd)                                    &
     &     = merged_fld%phys_name(1:num_field_ucd)
!
      ucd_header_name =     new_udt_head
      itype_ucd_data_file = itype_assembled_data
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        nnod_ucd = subdomains_2(ip)%node%numnod
        call allocate_ucd_node
        call allocate_ucd_phys_data
!
        call copy_domain_data_from_global(ip)
        call sel_write_udt_file(my_rank, istep)
!
        call deallocate_ucd_phys_data
        call deallocate_ucd_node
      end do
      call deallocate_ucd_phys_name
!
      end subroutine assemble_2nd_udt_phys
!
! -----------------------------------------------------------------------
!
      subroutine assemble_2nd_udt_nesh
!
      use m_file_format_switch
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint) :: ip, my_rank
!
!
      ucd_header_name =    new_udt_head
      itype_ucd_data_file = itype_assembled_data
      do ip = 1, num_pe2
        my_rank = ip - 1
!
        nnod_ucd = subdomains_2(ip)%node%numnod
        call allocate_ucd_node
        call copy_node_posi_from_global(ip)
!
        call count_udt_elements                                         &
     &     (subdomains_2(ip)%node%internal_node,                        &
     &      subdomains_2(ip)%ele%numele,                                &
     &      subdomains_2(ip)%ele%nnod_4_ele, subdomains_2(ip)%ele%ie)
        call allocate_ucd_ele
!
        call set_udt_global_connect                                     &
     &     (subdomains_2(ip)%node%internal_node,                        &
     &      subdomains_2(ip)%ele%numele,                                &
     &      subdomains_2(ip)%ele%nnod_4_ele,                            &
     &      subdomains_2(ip)%ele%iele_global, subdomains_2(ip)%ele%ie)
!
        call sel_write_grd_file(my_rank)
!
        call deallocate_ucd_node
!
        if(   mod(itype_ucd_data_file,100)/10 .eq. iflag_vtd/10         &
       & .or. mod(itype_ucd_data_file,100)/10 .eq. iflag_udt/10) then
          call deallocate_ucd_ele
        end if
      end do
!
      end subroutine assemble_2nd_udt_nesh
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_domain_data_from_global(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer (kind = kint) :: inod, inum
!
!
      do inum = 1, subdomains_2(ip)%node%numnod
        inod = subdomains_2(ip)%node%inod_global(inum)
        inod_gl_ucd(inum) = inod
        if (inod .le. merged%node%numnod) then
          d_nod_ucd(inum,1:ntot_comp_ucd)                               &
     &        = merged_fld%d_fld(inod,1:ntot_comp_ucd)
        else
          d_nod_ucd(inum,1:ntot_comp_ucd) = zero
        end if
      end do
!
      end subroutine copy_domain_data_from_global
!
! -----------------------------------------------------------------------
!
      subroutine copy_node_posi_from_global(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer (kind = kint) :: inum, inod
!
!
      do inum = 1, nnod_ucd
        inod = subdomains_2(ip)%node%inod_global(inum)
        inod_gl_ucd(inum) = inod
        xx_ucd(inum,1:3) = merged%node%xx(inod,1:3)
      end do
!
      end subroutine copy_node_posi_from_global
!
!  ---------------------------------------------------------------------
!
      end module output_newdomain_ucd
