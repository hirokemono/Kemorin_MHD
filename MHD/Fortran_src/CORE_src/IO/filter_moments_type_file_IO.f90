!filter_moments_type_file_IO.f90
!     module filter_moments_type_file_IO
!
!     programmed by H. Matsui on May, 2008
!
!      subroutine read_num_filter_mom_type_file(file_name, my_rank,     &
!     &          FEM_elens, FEM_moms)
!
!      subroutine read_filter_elen_type_file(file_name, my_rank,        &
!     &          nnod, nele, FEM_elens, ierr)
!      subroutine write_filter_elen_type_file(file_name, my_rank,       &
!     &          FEM_elens)
!
!      subroutine read_filter_moms_type_file(file_name, my_rank,        &
!     &          nnod, nele, FEM_elens, FEM_moms, ierr)
!      subroutine write_filter_moms_type_file(file_name, my_rank,       &
!     &          FEM_elens, FEM_moms)
!
!      subroutine read_num_filter_mom_type_file_b(file_name, my_rank,   &
!     &          FEM_elens, FEM_moms)
!
!      subroutine read_filter_elen_type_file_b(file_name, my_rank,      &
!     &          nnod, nele, FEM_elens, ierr)
!      subroutine write_filter_elen_type_file_b(file_name, my_rank,     &
!     &          FEM_elens)
!
!      subroutine read_filter_moms_type_file_b(file_name, my_rank,      &
!     &          nnod, nele, FEM_elens, FEM_moms, ierr)
!      subroutine write_filter_moms_type_file_b(file_name, my_rank,     &
!     &          FEM_elens, FEM_moms)
!        character(len=kchara), intent(in) :: file_name
!        integer(kind = kint), intent(in) :: my_rank
!        integer(kind = kint), intent(in) :: nnod, nele
!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!        type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!        integer(kind = kint), intent(inout) :: ierr
!
      module filter_moments_type_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_filter_elength
      use m_file_format_switch
      use filter_mom_type_data_IO
      use filter_mom_type_data_IO_b
      use set_parallel_file_name
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_num_filter_mom_type_file(file_name, my_rank,      &
     &          FEM_elens, FEM_moms)
!
      use t_filter_moments
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write ascii number of filter moms file: ',          &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name,                            &
     &        form='formatted', status= 'old')
      call read_filter_moment_num_type(filter_file_code,                &
     &      FEM_elens, FEM_moms)
      close(filter_file_code)
!
      end subroutine read_num_filter_mom_type_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_elen_type_file(file_name, my_rank,         &
     &          nnod, nele, FEM_elens, ierr)
!
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii filter length file: ',                   &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name,                            &
     &        form='formatted', status= 'old')
      call read_filter_elen_data_type(filter_file_code,                 &
     &      nnod, nele, FEM_elens, ierr)
      close(filter_file_code)
!
      end subroutine read_filter_elen_type_file
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_elen_type_file(file_name, my_rank,        &
     &          FEM_elens)
!
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write ascii filter length file: ',                  &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='formatted')
      call write_filter_elen_data_type(filter_file_code, FEM_elens)
      close(filter_file_code)
!
      end subroutine write_filter_elen_type_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_moms_type_file(file_name, my_rank,         &
     &          nnod, nele, FEM_elens, FEM_moms, ierr)
!
      use t_filter_moments
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii filter moment file: ',                   &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='formatted')
      call read_filter_moms_data_type(filter_file_code, nnod, nele,     &
     &      FEM_elens, FEM_moms, ierr)
      close(filter_file_code)
!
      end subroutine read_filter_moms_type_file
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_moms_type_file(file_name, my_rank,        &
     &          FEM_elens, FEM_moms)
!
      use t_filter_moments
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write ascii filter moment file: ',                  &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='formatted')
      call write_filter_moms_data_type(filter_file_code,                &
     &      FEM_elens, FEM_moms)
      close(filter_file_code)
!
      end subroutine write_filter_moms_type_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_num_filter_mom_type_file_b(file_name, my_rank,    &
     &          FEM_elens, FEM_moms)
!
      use t_filter_moments
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary number of filter moms file: ',         &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name,                            &
     &        form='unformatted', status= 'old')
      call read_filter_moment_num_type_b(filter_file_code,              &
     &      FEM_elens, FEM_moms)
      close(filter_file_code)
!
      end subroutine read_num_filter_mom_type_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_elen_type_file_b(file_name, my_rank,       &
     &          nnod, nele, FEM_elens, ierr)
!
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter length file: ',                  &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name,                            &
     &        form='unformatted', status= 'old')
      call read_filter_elen_data_type_b(filter_file_code,               &
     &        nnod, nele, FEM_elens, ierr)
      close(filter_file_code)
!
      end subroutine read_filter_elen_type_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_elen_type_file_b(file_name, my_rank,      &
     &          FEM_elens)
!
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter length file: ',                 &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='unformatted')
      call write_filter_elen_data_type_b(filter_file_code, FEM_elens)
      close(filter_file_code)
!
      end subroutine write_filter_elen_type_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_moms_type_file_b(file_name, my_rank,       &
     &          nnod, nele, FEM_elens, FEM_moms, ierr)
!
      use t_filter_moments
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter moment file: ',                  &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='unformatted')
      call read_filter_moms_data_type_b(filter_file_code,               &
     &      nnod, nele, FEM_elens, FEM_moms, ierr)
      close(filter_file_code)
!
      end subroutine read_filter_moms_type_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_moms_type_file_b(file_name, my_rank,      &
     &          FEM_elens, FEM_moms)
!
      use t_filter_moments
      use m_filter_file_names
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(gradient_filter_mom_type), intent(inout) :: FEM_moms
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter moment file: ',                 &
     &             trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='unformatted')
      call write_filter_moms_data_type_b(filter_file_code,              &
     &      FEM_elens, FEM_moms)
      close(filter_file_code)
!
      end subroutine write_filter_moms_type_file_b
!
!-----------------------------------------------------------------------
!
      end module filter_moments_type_file_IO
