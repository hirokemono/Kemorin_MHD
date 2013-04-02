!
!     module m_check_subroutines
!
!      Written by H. Matsui on Mar. 2005
!
!       subroutine check_reference_temp(my_rank)
!
      module m_check_subroutines
!
      use m_precision
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
       subroutine check_reference_temp(my_rank)
!
       use m_node_phys_address
       use m_node_phys_data
       use m_geometry_data_MHD
!
       integer(kind = kint) :: my_rank
       integer(kind = kint) :: inod, inum
!
       write(50+my_rank,*) 'inum, inod, ref_temp'
       do inum = 1, numnod_fluid
         inod = inod_fluid(inum)
         write(50+my_rank,*) inum, inod, d_nod(inod,iphys%i_ref_t)
       end do
!
       end subroutine check_reference_temp
!
!  --------------------------------------------------------------------
!
       subroutine check_numbers_of_nodes(my_rank)
!
       use m_geometry_data_MHD
!
       integer(kind = kint) :: my_rank
!
       if (my_rank .eq. 0 ) then
        write(12,*) 'numnod_fluid', numnod_fluid
        write(12,*) 'numnod_conduct', numnod_conduct
        write(12,*) 'numnod_insulate', numnod_insulate
        write(12,*) 'numnod_in_core', numnod_in_core
        write(12,*) 'numele_in_core', numele_in_core
       end if
!
       end subroutine check_numbers_of_nodes
!
!  --------------------------------------------------------------------
!
       subroutine check_nodes_4_layers(my_rank)
!
       use m_geometry_data_MHD
!
       integer(kind = kint) :: my_rank
!
        write(my_rank+50,*) 'inod_fluid'
        write(my_rank+50,'(10i8)') inod_fluid
        write(my_rank+50,*) 'inod_conduct'
        write(my_rank+50,'(10i8)') inod_conduct
        write(my_rank+50,*) 'inod_insulate'
        write(my_rank+50,'(10i8)') inod_insulate
        write(my_rank+50,*) 'inod_in_core'
        write(my_rank+50,'(10i8)') inod_in_core
!
       end subroutine check_nodes_4_layers
!
!  --------------------------------------------------------------------
!
!
      end module m_check_subroutines
