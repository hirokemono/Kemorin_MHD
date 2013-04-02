!
!      module interpolate_nodal_data
!
!      modified by H. Matsui on Sep., 2006 
!
!      subroutine s_interpolate_nodal_data
!
      module interpolate_nodal_data
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_nodal_data
!
      use m_parallel_var_dof
      use m_node_phys_data
      use m_2nd_phys_data
!
      use interpolate_scalar
      use interpolate_tensor
      use interpolate_vector
!
      integer(kind = kint) :: i, i_dest, i_origin
!
!
      do i = 1, num_nod_phys
        i_origin = istack_nod_component(i-1) + 1
        i_dest =   istack_nod_comps_2nd(i-1) + 1
        if      (num_nod_component(i) .eq. 1) then
          if (my_rank.eq.0) write(*,*) ' interpolate scalar: ',         &
     &            trim(phys_nod_name(i)), '  ', i_dest, i_origin
          call s_interpolate_scalar(i_dest, i_origin)
!
        else if (num_nod_component(i) .eq. 3) then
          if (my_rank.eq.0) write(*,*) ' interpolate vector: ',         &
     &            trim(phys_nod_name(i)), '  ', i_dest, i_origin
          call s_interpolate_vector(i_dest, i_origin)
!
        else if (num_nod_component(i) .eq. 6) then
          if (my_rank.eq.0) write(*,*) ' interpolate tensor: ',         &
     &            trim(phys_nod_name(i)), '  ', i_dest, i_origin
          call s_interpolate_tensor(i_dest, i_origin)
!
        end if
      end do
!
      end subroutine s_interpolate_nodal_data
!
! ----------------------------------------------------------------------
!
      end module interpolate_nodal_data
